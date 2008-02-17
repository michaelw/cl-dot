(defpackage class-hierarchy
  (:use :cl :cl-dot
        #+sbcl :sb-mop
        #-sbcl :closer-mop))

(in-package class-hierarchy)

(defmethod graph-object-node ((graph (eql 'class-example)) (object class))
  (make-instance 'node
                 :attributes (list :label (class-name object)
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod graph-object-points-to ((graph (eql 'class-example)) (object class))
  (class-direct-subclasses object))
