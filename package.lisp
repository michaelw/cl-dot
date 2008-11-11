;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(defpackage cl-dot
  (:use :common-lisp)
  (:export #:attributed #:node
           #:*dot-path*
           #:*neato-path*
           #:graph-object-knows-of #:graph-object-node
           #:graph-object-points-to #:graph-object-pointed-to-by
           #:generate-graph-from-roots
           #:print-graph #:dot-graph

           ;; deprecated
           #:object-knows-of #:object-node
           #:object-points-to #:object-pointed-to-by
           #:generate-graph))





