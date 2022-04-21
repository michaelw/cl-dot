;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(asdf:defsystem :cl-dot
  :version "0.9.0"
  :description "Generate Dot Output from Arbitrary Lisp Data"
  :author "Juho Snellman <jsnell@iki.fi>"
  :maintainer "Michael Weber <michaelw@foldr.org>"
  :depends-on (:uiop)
  :serial t
  :components
  ((:file "package")
   (:file "config-graphviz")
   (:file "attribute")
   (:file "raw-attributes") ; generated
   (:file "attributes")
   (:file "cl-dot")
   (:file "deprecated")
   (:static-file "README.md")
   (:static-file "COPYING")
   (:static-file "ChangeLog")
   (:module "examples"
    :components
    ((:static-file "class-example" :pathname "class-example.lisp")
     (:static-file "list-example" :pathname "list-example.lisp")
     (:static-file "list-example-old" :pathname "list-example-old.lisp")
     (:static-file "sb-c-example" :pathname "sb-c-example.lisp")
     (:static-file "subgraph-example" :pathname "subgraph-example.lisp")))))
