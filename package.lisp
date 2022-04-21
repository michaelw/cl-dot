;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(defpackage cl-dot
  (:use :common-lisp)
  (:export
   ;; Variables
   #:*dot-path*
   #:*neato-path*

   ;; Classes
   #:attributed
   #:node
   #:cluster
   #:edge
   #:graph

   ;; Accessors
   #:id-of
   #:attributes-of
   #:nodes-of
   #:edges-of
   #:source-of
   #:target-of

   ;; Graph Protocol
   #:graph-object-knows-of
   #:graph-object-node
   #:graph-object-cluster
   #:graph-object-points-to
   #:graph-object-pointed-to-by
   #:graph-object-edges
   #:generate-graph-from-roots

   ;; Miscellaneous
   #:print-graph
   #:dot-graph

   ;; Deprecated
   #:object-knows-of
   #:object-node
   #:object-points-to
   #:object-pointed-to-by
   #:generate-graph))





