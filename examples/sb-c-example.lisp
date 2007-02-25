(in-package cl-dot)

(defun list-no-nil (&rest args)
  (remove nil args))

;; COMPONENT
(defmethod object-node ((object sb-c:component))
  nil)

(defmethod object-knows-of ((c sb-c:component))
  (list* (sb-c::component-head c)
         (sb-c::component-lambdas c)))


;; CBLOCK
(defmethod object-node ((c sb-c::cblock))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Block ~A" (sb-c::block-number c)))))

(defmethod object-points-to ((c sb-c::cblock))
  (sb-c::block-succ c))

(defmethod object-knows-of ((c sb-c::cblock))
  (sb-c::block-pred c))

;; CLAMBDA
(defmethod object-node ((c sb-c::clambda))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Lambda ~A"
                                               (sb-c::lambda-%source-name c)))))

(defmethod object-points-to ((c sb-c::clambda))
  (sb-c::lambda-vars c))

;; LAMBDA-VAR
(defmethod object-node ((c sb-c::lambda-var))
  (make-instance 'node
                 :attributes `(:label ,(format nil "Var ~A"
                                               (sb-c::lambda-var-%source-name c)))))

(defmethod object-knows-of ((c sb-c::lambda-var))
  (sb-c::lambda-var-refs c))

;; REF
(defmethod object-node ((c sb-c::ref))
  (make-instance 'node
                 :attributes (list :label "REF"
                                   :fillcolor "#ddffbb"
                                   :style :filled
                                   :shape :diamond)))

(defmethod object-points-to ((c sb-c::ref))
  (list-no-nil (sb-c::ref-leaf c)
               (sb-c::ref-next c)))

(defmethod object-knows-of ((c sb-c::ref))
  (list-no-nil (sb-c::ref-prev c)
               (sb-c::ref-lvar c)))

;; CTRAN
(defmethod object-node ((c sb-c::ctran))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CTRAN ~A"
                                               (sb-c::ctran-kind c)))))

(defmethod object-points-to ((c sb-c::ctran))
  (list-no-nil (sb-c::ctran-next c)
               (sb-c::ctran-use c)))

;; BIND
(defmethod object-node ((c sb-c::bind))
  (make-instance 'node
                 :attributes `(:label ,(format nil "BIND"))))

(defmethod object-points-to ((c sb-c::bind))
  (list-no-nil (sb-c::bind-next c)))

(defmethod object-knows-of ((c sb-c::bind))
  (list-no-nil (sb-c::bind-prev c)))

;; GLOBAL-VAR
(defmethod object-node ((c sb-c::global-var))
  (make-instance 'node
                 :attributes (list :label (format nil "GLOBAL-VAR\\n~A\\n~A"
                                                  (sb-c::global-var-%source-name c)
                                                  (sb-c::global-var-kind c))
                                   :shape :box)))

(defmethod object-knows-of ((c sb-c::global-var))
  (sb-c::global-var-refs c))

;; CONSTANT
(defmethod object-node ((c sb-c::constant))
  (make-instance 'node
                 :attributes (list :label (format nil "CONSTANT ~A"
                                                  (sb-c::constant-%source-name c)
                                                  #+nil (sb-c::constant-value c))
                                   :style :filled
                                   :fillcolor "#ffffee"
                                   :shape :box)))

(defmethod object-knows-of ((c sb-c::constant))
  (sb-c::constant-refs c))

;; ENTRY
(defmethod object-node ((c sb-c::entry))
  (make-instance 'node
                 :attributes `(:label ,(format nil "ENTRY"))))

(defmethod object-points-to ((c sb-c::entry))
  ;; cleanup
  (list-no-nil (sb-c::entry-next c)))

(defmethod object-knows-of ((c sb-c::entry))
  (list-no-nil (sb-c::entry-prev c)))

;; COMBINATION
(defmethod object-node ((c sb-c::combination))
  (make-instance 'node
                 :attributes (list :label (format nil "COMBINATION")
                                   :shape :octagon
                                   :style :filled
                                   :fillcolor "#eeeeff")))

(defmethod object-points-to ((c sb-c::combination))
  (list-no-nil (sb-c::combination-next c)
               (sb-c::combination-lvar c)
               (sb-c::combination-fun c)))

(defmethod object-knows-of ((c sb-c::combination))
  (list* (sb-c::combination-prev c)
         (sb-c::combination-args c)))

;; LVAR
(defmethod object-node ((c sb-c::lvar))
  (make-instance 'node
                 :attributes (list :label (format nil "LVAR"
                                                  #+nil (sb-c::lvar-derived-type c))
                                   :style :filled
                                   :fillcolor "#ffeeff"
                                   :shape :octagon)))

(defmethod object-points-to ((c sb-c::lvar))
  (list-no-nil (sb-c::lvar-dest c)))

(defmethod object-pointed-to-by ((c sb-c::lvar))
  (let ((uses (sb-c::lvar-uses c)))
    (if (listp uses)
        uses
        (list-no-nil uses))))

;; CIF
(defmethod object-node ((c sb-c::cif))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CIF"))))

(defmethod object-points-to ((c sb-c::cif))
  (list-no-nil (sb-c::if-next c)
               (sb-c::if-test c)
               (sb-c::if-consequent c)
               (sb-c::if-alternative c)))

(defmethod object-knows-of ((c sb-c::cif))
  (list-no-nil (sb-c::if-prev c)))

;; CRETURN
(defmethod object-node ((c sb-c::creturn))
  (make-instance 'node
                 :attributes `(:label ,(format nil "CRETURN"))))

(defmethod object-points-to ((c sb-c::creturn))
  (list-no-nil (sb-c::return-next c)))

(defmethod object-knows-of ((c sb-c::creturn))
  (list-no-nil (sb-c::return-prev c)
               (sb-c::return-result c)))
