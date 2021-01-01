(defsystem "rummage"
  :version "1.0.0"
  :author "Armon Toubman"
  :license "MIT"
  :depends-on ("dexador"
               "lparallel"
               "cl-generator"
               "lquery"
               "alexandria"
               "quri"
               "log4cl"
               "bt-semaphore")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Rummage is a small web-scraping library"
  :in-order-to ((test-op (test-op "rummage/tests"))))

(defsystem "rummage/tests"
  :author "Armon Toubman"
  :license "Expat"
  :depends-on ("rummage"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for rummage"
  :perform (test-op (op c) (symbol-call :rove :run c)))
