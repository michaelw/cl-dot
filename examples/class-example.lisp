(defpackage class-hierarchy
  (:use :cl :cl-dot
        #+sbcl :sb-mop
        #-(or sbcl)
        (error "Don't know the MOP package for this implementation")))

(in-package class-hierarchy)

(defmethod object-node ((object class))
  (make-instance 'node
                 :attributes (list :label (class-name object)
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod object-points-to ((object class))
  (class-direct-subclasses object))




