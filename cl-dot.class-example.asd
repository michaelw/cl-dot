;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(asdf:defsystem :cl-dot.class-example
  :version "0.8.0"
  :description "Generate Dot Output from Arbitrary Lisp Data"
  :author "Juho Snellman <jsnell@iki.fi>"
  :maintainer "Michael Weber <michaelw@foldr.org>"
  :depends-on (:cl-dot)
  :pathname "examples/"
  :serial t
  :long-description "

This example contains a convenient functions `visualize-class-hierarchy'
and `visualize-class-hierarchy-in-package'. These apps help you understand
the large systems with complicated CLOS hierarchy e.g. ASDF, CLACK, CXML and so on.

Try this:

 (class-hierarchy:visualize-class-hierarchy-in-package
   (merge-pathnames \"asdf-classes.png\" (user-homedir-pathname)) 'asdf)
"
  :components
  ((:file "class-example"))
  :perform (load-op :after (op c) (princ (asdf:system-long-description c))))


