;;;; -*- Mode: Lisp -*-

(defpackage #:ltd-asd (:use :cl))
(in-package #:ltd-asd)

(asdf:defsystem :lisp-to-dylan
  :description "Common Lisp to Dylan translator"
  :version "0.1"
  :author "Peter Norvig"
  :maintainer "Peter S. Housel <housel@acm.org>"
  :serial t ;; for now...
  :pathname "code/"
  :components ((:file "package")
               (:file "misc")
               (:file "options")
               (:file "read")
               (:file "dpp")
               (:file "ltd")
               (:file "ltd-table")
               (:file "loop")
               (:file "tables"))
  :in-order-to ((asdf:test-op (asdf:test-op :lisp-to-dylan/test))))

(asdf:defsystem :lisp-to-dylan/test
  :author "Peter Norvig"
  :description "lisp-to-dylan test suite"
  :pathname "code/"
  :components ((:file "run-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :ltd-test :run-tests)))
