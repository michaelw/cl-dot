;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(defpackage :cl-dot-system (:use #:asdf #:cl))
(in-package :cl-dot-system)

(defsystem :cl-dot
  :version "0.1"
  :serial t
  :components
  ((:file "package")
   (:file "attributes")
   (:file "cl-dot")))
