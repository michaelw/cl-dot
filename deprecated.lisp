;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package cl-dot)

;;;; Deprecated Functionality

(defgeneric object-node (object)
  (:documentation
   "Return a NODE instance for this object, or NIL. In the latter case
the object will not be included in the graph, but it can still have an
indirect effect via other protocol functions (e.g. OBJECT-KNOWS-OF).
This function will only be called once for each object during the
generation of a graph."))

(defgeneric object-points-to (object)
  (:documentation
   "Return a list of objects to which the NODE of this object should be
connected. The edges will be directed from this object to the others.
To assign dot attributes to the generated edges, each object can optionally
be wrapped in a instance of ATTRIBUTED.")
  (:method ((object t))
    nil))

(defgeneric object-pointed-to-by (object)
  (:documentation
   "Return a list of objects to which the NODE of this object should be
connected. The edges will be directed from the other objects to this
one. To assign dot attributes to the generated edges, each object can
optionally be wrapped in a instance of ATTRIBUTED.")
  (:method ((object t))
    nil))

(defgeneric object-knows-of (object)
  (:documentation
   "Return a list of objects that this object knows should be part of the
graph, but which it has no direct connections to.")
  (:method ((object t))
    nil))

(defgeneric generate-graph (object &optional attributes)
  (:documentation "Construct a GRAPH with ATTRIBUTES starting
from OBJECT, using the GRAPH-OBJECT- protocol.")
  (:method ((object t) &optional attributes)
    (generate-graph-from-roots 'default (list object) attributes)))
