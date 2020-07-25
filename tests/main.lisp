(defpackage rummage/tests/main
  (:use :cl
        :rummage
        :rove))
(in-package :rummage/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :rummage)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
