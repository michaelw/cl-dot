;; -*- Syntax: Ansi-Common-Lisp; Mode: lisp; -*-

(defpackage cl-dot
  (:use :common-lisp)
  (:export #:attributed #:node
           #:*dot-path*
           #:object-knows-of #:object-node
           #:object-points-to #:object-pointed-to-by
           #:generate-graph #:print-graph #:dot-graph))





