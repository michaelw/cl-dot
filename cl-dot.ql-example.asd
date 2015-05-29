;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(asdf:defsystem :cl-dot.ql-example
  :version "0.8.0"
  :description "Generate Dot Output from Package, Class, etc..."
  :author "Juho Snellman <jsnell@iki.fi>"
  :maintainer "Michael Weber <michaelw@foldr.org>"
  :depends-on (:cl-dot)
  :pathname "examples/"
  :serial t
  :long-description "

This example contains convenient functions `visualize-ql-hierarchy'.

Try this (assume quicklisp is already loaded):

 (ql-hierarchy:visualize-ql-hierarchy
   (merge-pathnames \"ql-classes.png\" (user-homedir-pathname)))
"
  :components
  ((:file "ql-example"))
  :perform (load-op :after (op c) (princ (asdf:system-long-description c))))


