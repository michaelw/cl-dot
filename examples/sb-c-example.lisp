(in-package cl-dot)

(defun list-no-nil (&rest args)
  (remove nil args))

;; COMPONENT
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (object sb-c:component))
  nil)

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c:component))
  (list* (sb-c::component-head c)
         (sb-c::component-lambdas c)))


;; CBLOCK
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::cblock))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Block ~A" (sb-c::block-number c)))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::cblock))
  (sb-c::block-succ c))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::cblock))
  (sb-c::block-pred c))

;; CLAMBDA
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::clambda))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Lambda ~A"
                                               (sb-c::lambda-%source-name c)))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::clambda))
  (sb-c::lambda-vars c))

;; LAMBDA-VAR
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::lambda-var))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Var ~A"
                                               (sb-c::lambda-var-%source-name c)))))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::lambda-var))
  (sb-c::lambda-var-refs c))

;; REF
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::ref))
  (make-instance 'node
                 :attributes (list :label "REF"
                                   :fillcolor "#ddffbb"
                                   :style :filled
                                   :shape :diamond)))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::ref))
  (list-no-nil (sb-c::ref-leaf c)
               (sb-c::ref-next c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::ref))
  (list-no-nil (sb-c::ref-prev c)
               (sb-c::ref-lvar c)))

;; CTRAN
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::ctran))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CTRAN ~A"
                                               (sb-c::ctran-kind c)))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::ctran))
  (list-no-nil (sb-c::ctran-next c)
               (sb-c::ctran-use c)))

;; BIND
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::bind))
  (make-instance 'node
                 :attributes `(:label ,(format nil "BIND"))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::bind))
  (list-no-nil (sb-c::bind-next c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::bind))
  (list-no-nil (sb-c::bind-prev c)))

;; GLOBAL-VAR
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::global-var))
  (make-instance 'node
                 :attributes (list :label (format nil "GLOBAL-VAR\\n~A\\n~A"
                                                  (sb-c::global-var-%source-name c)
                                                  (sb-c::global-var-kind c))
                                   :shape :box)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::global-var))
  (sb-c::global-var-refs c))

;; CONSTANT
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::constant))
  (make-instance 'node
                 :attributes (list :label (format nil "CONSTANT ~A"
                                                  (sb-c::constant-%source-name c)
                                                  #+nil (sb-c::constant-value c))
                                   :style :filled
                                   :fillcolor "#ffffee"
                                   :shape :box)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::constant))
  (sb-c::constant-refs c))

;; ENTRY
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::entry))
  (make-instance 'node
                 :attributes `(:label ,(format nil "ENTRY"))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::entry))
  ;; cleanup
  (list-no-nil (sb-c::entry-next c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::entry))
  (list-no-nil (sb-c::entry-prev c)))

;; COMBINATION
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::combination))
  (make-instance 'node
                 :attributes (list :label (format nil "COMBINATION")
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::combination))
  (list-no-nil (sb-c::combination-next c)
               (sb-c::combination-lvar c)
               (sb-c::combination-fun c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::combination))
  (list* (sb-c::combination-prev c)
         (sb-c::combination-args c)))

;; LVAR
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::lvar))
  (make-instance 'node
                 :attributes (list :label (format nil "LVAR"
                                                  #+nil (sb-c::lvar-derived-type c))
                                   :style :filled
                                   :fillcolor "#ffeeff"
                                   :shape :octagon)))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::lvar))
  (list-no-nil (sb-c::lvar-dest c)))

(defmethod graph-object-pointed-to-by ((graph (eql 'sb-c-example)) (c sb-c::lvar))
  (let ((uses (sb-c::lvar-uses c)))
    (if (listp uses)
        uses
        (list-no-nil uses))))

;; CIF
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::cif))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CIF"))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::cif))
  (list-no-nil (sb-c::if-next c)
               (sb-c::if-test c)
               (sb-c::if-consequent c)
               (sb-c::if-alternative c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::cif))
  (list-no-nil (sb-c::if-prev c)))

;; CRETURN
(defmethod graph-object-node ((graph (eql 'sb-c-example)) (c sb-c::creturn))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CRETURN"))))

(defmethod graph-object-points-to ((graph (eql 'sb-c-example)) (c sb-c::creturn))
  (list-no-nil (sb-c::return-next c)))

(defmethod graph-object-knows-of ((graph (eql 'sb-c-example)) (c sb-c::creturn))
  (list-no-nil (sb-c::return-prev c)
               (sb-c::return-result c)))
