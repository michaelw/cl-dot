#|

This example contains convenient functions `visualize-ql-hierarchy'.

Try this (assume quicklisp is already loaded):

 (ql-hierarchy:visualize-ql-hierarchy
   (merge-pathnames \"asdf-classes.png\" (user-homedir-pathname)))

|#


(defpackage :ql-hierarchy
  (:use :cl :cl-dot
        #+sbcl :sb-mop
        #-sbcl :closer-mop)
  (:export
   #:ql-example
   #:visualize-ql-hierarchy))

(in-package :ql-hierarchy)

(defmethod graph-object-node ((graph (eql 'ql-example)) (object QL-DIST:SYSTEM))
  (make-instance 'node
                 :attributes (list :label (ql-dist:name object)
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod graph-object-points-to ((graph (eql 'ql-example)) (object QL-DIST:SYSTEM))
  (remove nil
          (mapcar #'ql-dist:find-system
                  (ql-dist:required-systems object))))

(defun visualize-ql-hierarchy (target-png &optional (seed-systems (ql:system-list)))
  (dot-graph
   (generate-graph-from-roots 'ql-example seed-systems '(:rankdir "LR"))
   target-png :format :png))


