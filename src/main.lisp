(defpackage rummage
  (:use :cl
        :alexandria
        :generator
        :quri
        :log4cl
        :lparallel)
  (:local-nicknames (:alex :alexandria)
                    (:gen :generator))
  (:export #:scrape
           #:follow))
(in-package :rummage)

(defparameter *request-lock* (bt:make-lock))

(defun request (ctx url)
  (bt:with-lock-held (*request-lock*)
    (sleep (+ (getf ctx :request-delay) (if (plusp (getf ctx :request-delay-variance))
                                            (random (getf ctx :request-delay-variance))
                                            0)))
    ;; TODO: Understand conditions and handlers.
    (log:info "Requesting <~a>" url)
    (handler-case (apply #'dex:get url (getf ctx :dex-opts))
      (dex:http-request-bad-request (e)
        ;; Runs when 400 bad request returned
        (log:info "Request failed <~a ~a>"
                (dex:response-status e) url)
        (values nil e))
      (dex:http-request-failed (e)
        ;; For other 4xx or 5xx
        (log:info "Request failed <~a ~a>"
                (dex:response-status e) url)
        (values nil e))
      (t (e)
        (log:info "Request failed for some reason: ~a" (type-of e))
        (values nil e)))))

(gen:defun* scrape-url (ctx url parsefun)
  (let* ((ctx (copy-list ctx))
         (res (setf (getf ctx :current-url) url))
         (response (request ctx url)))
    (when response
      ; call the parsefun to determine what kind it is based on the return value
      (let ((retval (funcall parsefun ctx response)))
        (typecase retval
          (gen:iter (gen:yield* retval)) ; re-yield items from the parsefun
          (t (yield retval))))))) ; pretend-yield whatever parsefun returns

(defun process-pipeline (item pipeline)
  (when (and item pipeline)
    ; call the pipe to determine what kind it is based on the return value
    (let ((retval (funcall (car pipeline) item)))
      (typecase retval
        ; is the pipe a defun* that yields stuff?
        (gen:iter (let* ((source retval)
                         (current (funcall (gen:iter-next source))))
                    (loop until (null (gen:iter-next source))
                      do (progn
                           ; whatever is yielded goes into the rest of the pipeline
                           (process-pipeline current (cdr pipeline))
                           (setf current (funcall (gen:iter-next source)))))))
        ; is the pipe a regular function?
        (t (process-pipeline retval (cdr pipeline)))))))

(defun scrape (start-url
               parsefun
               &key
               (pipeline (list (lambda (item) (log:info item))))
               (workers 1)
               (request-delay 3)
               (request-delay-variance 0)
               (dex-opts '()))
  (log:info "Starting scrape of <~a>.~%" start-url)
  (log:info "Starting kernel with ~a workers.~%" workers)
  (setf lparallel:*kernel* (lparallel:make-kernel workers :name "rummage-kernel"))
  (let* ((ctx (list :start-url (quri:uri start-url)
                    :dex-opts dex-opts
                    :request-delay request-delay
                    :request-delay-variance request-delay-variance))
         (source (scrape-url ctx start-url parsefun))
         (futures '())
         (task-counter 0)
         (channel (make-channel)))
    (typecase source
      (gen:iter (let ((current (funcall (gen:iter-next source))))
                  (loop until (null (gen:iter-next source))
                    do (progn
                         (submit-task channel #'process-pipeline current pipeline)
                         (setf current (funcall (gen:iter-next source)))))
                  (dotimes (i task-counter)
                    (receive-result channel))))
      (t (log:info "No items were yielded by parsefun."))))
  (log:info "Asking kernel to end...~%")
  (lparallel:end-kernel :wait t)
  (log:info "Kernel ended.~%")
  (log:info "Completed scrape of <~a>.~%" start-url))

(gen:defun* follow (ctx url parsefun)
  (let ((abs-url (quri:render-uri (quri:merge-uris (quri:uri url) (getf ctx :start-url)))))
        ; relative urls are merged onto the start-url
      (gen:yield* (scrape-url ctx abs-url parsefun))))
