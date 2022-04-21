(defpackage subgraph-example
  (:use :cl :cl-dot
        #+sbcl :sb-mop
        #-sbcl :closer-mop))

(in-package subgraph-example)

;; A hash table, mapping from classes to clusters.
(defvar *class-cluster*)

(defmethod generate-graph-from-roots
    ((graph (eql 'subgraph-example)) objects &optional attributes)
  (let ((*class-cluster* (make-hash-table)))
    ;; The :compound attribute is required to allow edges between clusters.
    (call-next-method graph objects (list* :compound t attributes))))

(defmethod graph-object-node ((graph (eql 'subgraph-example)) (object class))
  (make-instance 'node
    :attributes (list :label (class-name object)
                      :shape :octagon
                      :style :filled
                      :fillcolor "#eeeeff")))

(defmethod graph-object-cluster ((graph (eql 'subgraph-example)) (object class))
  (or (gethash (class-of object) *class-cluster*)
      (setf (gethash (class-of object) *class-cluster*)
            (make-instance 'cluster
              :attributes (list :label (class-name (class-of object)))))))

(defmethod graph-object-points-to ((graph (eql 'subgraph-example)) (object class))
  (class-direct-subclasses object))

#+(or) ;; A quick demo:
(dot-graph (generate-graph-from-roots 'subgraph-example (list (find-class 'function)))
           "subgraph-plot.pdf" :format :pdf)
