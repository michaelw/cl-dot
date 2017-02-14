(cl:in-package #:cl-dot)

(deftype context ()
  "A context in which an attribute may occur."
  '(member :graph :subgraph :cluster :node :edge))

(defun context-list-p (thing)
  (and (listp thing)
       (every (lambda (element) (typep element 'context)) thing)))

(deftype context-set ()
  "A set of contexts in which an attribute may occur."
  '(satisfies context-list-p))

(defun foreign-name->lisp-name (name)
  "Return an idiomatic Lisp name derived from the GraphViz name NAME."
  (intern (string-upcase (substitute #\- #\_ name)) :keyword))

(defstruct (attribute
             (:constructor make-attribute (foreign-name allowed-in type
                                           &aux
                                           (name (foreign-name->lisp-name
                                                  foreign-name))))
             (:predicate nil)
             (:copier nil))
  "Description of a GraphViz attribute."
  (name         nil :type symbol           :read-only t)
  (foreign-name nil :type string           :read-only t)
  (allowed-in   nil :type context-set      :read-only t)
  (type         nil :type (or symbol cons) :read-only t))
