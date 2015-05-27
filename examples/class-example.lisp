#|

This example contains convenient functions `visualize-class-hierarchy'
and `visualize-class-hierarchy-in-package'. These apps help you understand
large systems with complicated CLOS hierarchy e.g. ASDF, CLACK, CXML and so on.

Try this:

 (class-hierarchy:visualize-class-hierarchy-in-package
   (merge-pathnames "asdf-classes.png" (user-homedir-pathname)) 'asdf)

|#


(defpackage :class-hierarchy
  (:use :cl :cl-dot
        #+sbcl :sb-mop
        #-sbcl :closer-mop)
  (:export
   #:class-example
   #:visualize-class-hierarchy
   #:visualize-class-hierarchy-in-package))

(in-package :class-hierarchy)

(defmethod graph-object-node ((graph (eql 'class-example)) (object class))
  (make-instance 'node
                 :attributes (list :label (class-name object)
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod graph-object-points-to ((graph (eql 'class-example)) (object class))
  (class-direct-subclasses object))

(defun visualize-class-hierarchy (target-png &optional (seed-classes '(t)))
  (dot-graph
   (generate-graph-from-roots
    'class-example
    (remove nil (mapcar (lambda (x) (if (typep x 'class) x (find-class x nil))) seed-classes))
    '(:rankdir "LR"))
   target-png :format :png))

(defun visualize-class-hierarchy-in-package (target-png pkg-designator)
  (visualize-class-hierarchy
   target-png
   (let (acc)
     (do-external-symbols (s (find-package pkg-designator) acc)
       (let ((c (find-class s nil)))
         (when c (print c) (push c acc)))))))
