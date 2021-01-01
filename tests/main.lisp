(defpackage rummage/tests/main
  (:use :cl
        :alexandria
        :quri
        :generator
        :rummage
        :rove))
(in-package :rummage/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :rummage)' in your Lisp.

;; NOTE: To see debug logs, use `(log:config :debug)'.

;;;;;;;;;; Quickstart example.

;; Prepare a parser.
; Use of defun* gives access to yield (see :generator).
(defun* quickstart-parser (ctx response)
  ; We expect an HTML response, so we parse it with lquery.
  (let ((doc (lquery:$ (initialize response))))
    ; Get the required information out of matching elements.
    (loop for author across (lquery:$ doc "small.author" (text))
      ; Yield the extracted information as an 'item' (e.g. a plist).
      do (yield (list :author author)))
    ; If there is a link to the next page,
    (let ((next-page (lquery:$ doc "li.next a" (attr :href))))
      (when (not (emptyp next-page))
        ; we follow the link and parse the response.
        ; We re-yield any newly extracted items.
        (yield* (rummage:follow ctx (elt next-page 0) #'quickstart-parser))))))

;; Extracted items are passed through a pipeline of functions.
;; Prepare a function that prints the author in each item.
(defun print-author (item)
  ; Do something with the passed item.
  (format t "~a~%" (getf item :author))
  ; Pass it on to the next function in the pipeline.
  item)

(deftest quickstart-test
  (testing "Test the quickstart code from the README."
           (rummage:scrape "http://quotes.toscrape.com"
                   #'quickstart-parser
                   :pipeline (list #'print-author))))

;;;;;;;;;; End quickstart example.

(defun* parse-quotes.toscrape.com (ctx response)
  (let ((doc (lquery:$ (initialize response))))
    (loop for quote across (lquery:$ doc "div.quote")
      do (yield (list :author (lquery:$ quote "span>small" (text) #'first-elt)
                      :text (lquery:$ quote "span.text" (text) #'first-elt))))
    (let ((next-page (lquery:$ doc "li.next a" (attr :href))))
      (when (not (emptyp next-page))
        (yield* (follow ctx (first-elt next-page) #'parse-quotes.toscrape.com))))))

(defun non-yielding-parsefun (ctx response)
  (declare (ignore ctx))
  (declare (ignore response))
  :some-return-value)

(defun* item-doubling-pipe (item)
  (yield item)
  (yield item))

(defun slow-pipe (item)
  (sleep 1)
  item)

(defvar *list-lock* (bt:make-lock "*list-lock*"))
(defvar *count-lock* (bt:make-lock "*count-lock*"))

(deftest test-quotes.toscrape.com
  (testing "Scrape 100 quotes from quotes.toscrape.com."
           (time
             (let ((quotes '()))
               (scrape "http://quotes.toscrape.com/"
                       #'parse-quotes.toscrape.com
                       :pipeline (list
                                  (lambda (item)
                                    (bt:with-lock-held (*list-lock*)
                                      (push item quotes))
                                    nil))
                       :workers 1
                       :request-delay 0)
               (ok (= 100 (length quotes)))
               (ng (member nil quotes))))))

(deftest test-quotes.toscrape.com-non-yielding-parsefun
  (testing "A non-yielding parser does not scrape anything."
           (time
             (let ((quotes '())
                   (called nil))
               (scrape "http://quotes.toscrape.com/"
                       #'non-yielding-parsefun ; whatever it returns goes into the pipeline
                       :pipeline (list
                                  (lambda (item)
                                    (bt:with-lock-held (*list-lock*)
                                      (push item quotes)
                                      (setf called t))
                                    nil))
                       :workers 1
                       :request-delay 0)
               (ok (= 1 (length quotes)))
               (ok (member :some-return-value quotes))
               (ok called)))))

(deftest test-quotes.toscrape.com-pipeline
  (testing "Yielding new items within the pipeline."
           (time
             (let ((quotes '())
                   (count 0))
               (scrape "http://quotes.toscrape.com/"
                       #'parse-quotes.toscrape.com
                       :pipeline (list
                                  (lambda (item)
                                    (bt:with-lock-held (*count-lock*)
                                      (incf count))
                                    item)
                                  #'item-doubling-pipe
                                  #'slow-pipe
                                  (lambda (item)
                                    (bt:with-lock-held (*list-lock*)
                                      (push item quotes))))
                       :workers 16
                       :request-delay 0)
               (ok (= 100 count))
               (ok (= 200 (length quotes)))))))

(defun* parse-httpstat.us (ctx response)
  (null ctx) ; can't (declare (ignore ctx)) in defun* apparently
  (yield (lquery:$ (lquery:$ (initialize response)) "html" (text))))

(deftest test-ignore-404
  (testing "Failed HTTP requests should be ignored."
           (let ((called nil))
              (scrape "http://httpstat.us/404"
                      #'parse-httpstat.us
                      :pipeline (list
                                 (lambda (item)
                                   (declare (ignore item))
                                   (setf called t))))
             (ng called))))

(deftest test-ignore-500
  (testing "Failed HTTP requests should be ignored."
           (let ((called nil))
              (scrape "http://httpstat.us/500"
                      #'parse-httpstat.us
                      :pipeline (list
                                 (lambda (item)
                                   (declare (ignore item))
                                   (setf called t))))
             (ng called))))
