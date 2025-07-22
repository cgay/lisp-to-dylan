(defpackage #:ltd-test (:use :cl))
(in-package #:ltd-test)

;;; Some packages that need to exist for the reader to not blow up.
(defpackage comp)
(defpackage dtp)
(defpackage mma)
(defpackage excl)
(defpackage system)
(defpackage user)

(defun run-tests ()
  (let* ((path (merge-pathnames #P"test/*.lisp"
                                (asdf:system-source-directory "lisp-to-dylan")))
         (files (directory path)))
    (ltd:ltd-files files)))
