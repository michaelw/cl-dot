;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(asdf:defsystem :cl-dot.asdf-example
  :version "0.8.0"
  :description "Generate Dot Output from Package, Class, etc..."
  :author "Juho Snellman <jsnell@iki.fi>"
  :maintainer "Michael Weber <michaelw@foldr.org>"
  :depends-on (:cl-dot :trivia)
  :pathname "examples/"
  :serial t
  :long-description "

This example contains convenient functions `visualize-asdf-hierarchy'.

Try this (assume quicklisp is already loaded):

 (asdf-hierarchy:visualize-asdf-hierarchy
   (merge-pathnames \"asdf-systems.png\" (user-homedir-pathname)))
"
  :components
  ((:file "asdf-example"))
  :perform (load-op :after (op c) (princ (asdf:system-long-description c))))


