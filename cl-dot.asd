;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(asdf:defsystem :cl-dot
  :version "0.8.0"
  :description "Generate Dot Output from Arbitrary Lisp Data"
  :author "Juho Snellman <jsnell@iki.fi>"
  :maintainer "Michael Weber <michaelw@foldr.org>"
  :serial t
  :components
  ((:file "package")
   (:file "attributes")
   (:file "cl-dot")
   (:file "deprecated")
   (:static-file "README")
   (:static-file "COPYING")
   (:static-file "ChangeLog")
   (:module "examples"
            :components
            ((:static-file "class-example" :pathname "class-example.lisp")
             (:static-file "list-example" :pathname "list-example.lisp")
             (:static-file "list-example-old" :pathname "list-example-old.lisp")
             (:static-file "sb-c-example" :pathname "sb-c-example.lisp")))
   (:module "docs"
            :components
            ((:html-file "index")))))
