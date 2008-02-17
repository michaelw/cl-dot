(in-package cl-dot)

;; Conses
(defmethod graph-object-node ((graph (eql 'list-example)) (object cons))
  (make-instance 'node :attributes (list :label "cell \\N"
                                         :shape :box)))

(defmethod graph-object-points-to ((graph (eql 'list-example)) (object cons))
  (list (car object)
        (make-instance 'attributed
                       :object (cdr object)
                       :attributes (list :weight 3))))

;; Symbols
(defmethod graph-object-node ((graph (eql 'list-example)) (object symbol))
  (make-instance 'node :attributes (list :label object
                                         :shape :hexagon
                                         :style :filled
                                         :color :black
                                         :fillcolor "#ccccff")))
