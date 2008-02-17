(in-package cl-dot)

;; Conses
(defmethod object-node ((object cons))
  (make-instance 'node :attributes (list :label "cell \\N"
                                         :shape :box)))

(defmethod object-points-to ((object cons))
  (list (car object)
        (make-instance 'attributed
                       :object (cdr object)
                       :attributes (list :weight 3))))

;; Symbols
(defmethod object-node ((object symbol))
  (make-instance 'node :attributes (list :label object
                                         :shape :hexagon
                                         :style :filled
                                         :color :black
                                         :fillcolor "#ccccff")))
