(defsystem "rummage"
  :version "0.1.0"
  :author "Armon Toubman"
  :license "Expat"
  :depends-on ("dexador"
               "lparallel")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
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
