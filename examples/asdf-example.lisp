#|

This example contains convenient functions `visualize-asdf-hierarchy'.

Try this (assume quicklisp is already loaded):

 (asdf-hierarchy:visualize-asdf-hierarchy
   (merge-pathnames \"asdf-systems.png\" (user-homedir-pathname)))

|#


(defpackage :cl-dot.asdf-example
  (:use :cl :cl-dot :trivia
        #+sbcl :sb-mop
        #-sbcl :closer-mop)
  (:export
   #:dependson
   #:visualize-asdf-hierarchy))

(in-package :cl-dot.asdf-example)

(defmethod graph-object-node ((graph (eql 'dependson)) (object asdf:component))
  (make-instance 'node
                 :attributes (list :label (asdf:component-name object)
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defun dependency-name (dependency-def)
  #|
  https://common-lisp.net/project/asdf/asdf.html#The-defsystem-grammar
  dependency-def := simple-component-name
  | ( :feature feature-expression dependency-def )
  | ( :version simple-component-name version-specifier )
  | ( :require module-name )
  |#
  (ematch dependency-def
    ((list* :feature _ rest) (dependency-name rest))
    ((list :version name _) name)
    ((list :require module-name) module-name)
    (name name)))

(defmethod graph-object-points-to ((graph (eql 'dependson)) (object asdf:system))
  (remove nil
          (mapcar (lambda (dependency-def)
                    (asdf:find-system (dependency-name dependency-def)))
                  (asdf:system-depends-on object))))


(defun visualize-asdf-hierarchy (target-png &optional (seed-systems (asdf:registered-systems)) (mode 'dependson))
  (dot-graph
   (generate-graph-from-roots mode seed-systems '(:rankdir "LR"))
   target-png :format :png))


