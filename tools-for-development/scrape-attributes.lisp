;;; Usage:
;;;
;;; (ql:quickload '(:alexandria :drakma :cxml :cxml-stp :xml.location :closure-html :cl-dot))
;;; (load "scrape-attributes.lisp")
;;; (cl-dot.attribute-scraper:write-attributes-definition)

(cl:defpackage #:cl-dot.attribute-scraper
  (:use
   #:cl
   #:alexandria)

  (:import-from #:cl-dot
   #:text
   #:label-text)

  (:export
   #:write-attributes-definition))

(cl:in-package #:cl-dot.attribute-scraper)

;;; Parameters

(defvar *attribute-document-url*
  "http://www.graphviz.org/doc/info/attrs.html")

(defvar *shapes-document-url*
  "http://www.graphviz.org/doc/info/shapes.html")

(defvar *arrows-document-url*
  "http://www.graphviz.org/doc/info/arrows.html")

(defvar *output-file*
  (asdf:system-relative-pathname :cl-dot "raw-attributes.lisp"))

;;; Obtaining and parsing the document

(defvar *whitespace-characters*
  '(#\Space #\Tab #\Newline))

(defvar *xhtml-namespaces*
  '((nil . "http://www.w3.org/1999/xhtml")))

(defun parse-document (content)
  (closure-html:parse content (stp:make-builder)))

(defun obtain-document (url)
  (parse-document (drakma:http-request url)))

;;; Scraping of attributes
;;;
;;; Find rows in the attributes table. For each row, extract the name,
;;; the graph elements it is allowed in and its type.

(defun scrape-attribute-definitions (document)
  (xloc:with-locations-r/o
      (((:val attributes :type 'attribute-description) "//table[@align=\"CENTER\"]/tbody/tr[td]"
        :if-multiple-matches :all)
       :namespaces *xhtml-namespaces*)
    document
    attributes))

(defmethod xloc:xml-> ((value stp:node)
                       (type  (eql 'attribute-description))
                       &key inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o
      (((:val name)                                   "td[1]/a/text()")
       ((:val allowed-in :type 'attribute-allowed-in) "td[2]/text()")
       ((:val type :type 'attribute-type)             "td[3]")
       :namespaces *xhtml-namespaces*)
      value
    (list name allowed-in type)))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'attribute-allowed-in))
                       &key inner-types)
  (declare (ignore inner-types))
  (map 'list (lambda (character)
               (ecase character
                 (#\G :graph)
                 (#\S :subgraph)
                 (#\C :cluster)
                 (#\N :node)
                 (#\E :edge)))
       value))

(defmethod xloc:xml-> ((value stp:node)
                       (type  (eql 'attribute-type))
                       &key inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o ((types ".//text()" :if-multiple-matches :all)
                            :namespaces *xhtml-namespaces*)
      value
    (flet ((only-whitespace-p (string)
             (every (rcurry #'member *whitespace-characters*) string)))
      (let ((types    (remove-duplicates
                       (mapcar #'graphviz-type->lisp-type
                               (remove-if #'only-whitespace-p types))))
            (fallback ''text))
        (cond
          ((= 1 (length types))
           (first types))
          (t
           (warn "~@<Cannot represent union type of ~{~S~^ and ~}, ~
                  falling back to ~S~@:>"
                 types fallback)
           fallback))))))

(defun graphviz-type->lisp-type (string)
  (eswitch (string :test #'string=)
    ("bool"        ''boolean)
    ("int"         ''integer)
    ("double"      ''float)
    ("string"      ''text)

    ("escString"   ''text)

    ("arrowType"   'cl-dot::*predefined-arrow-shapes*)
    ("rect"        ''text)

    ("color"       ''text)
    ("colorList"   ''text)

    ("clusterMode" ''(:local :global :none))
    ("dirType"     ''(:forward :back :both :none))
    ("addDouble"   ''text)
    ("addPoint"    ''text)
    ("point"       ''text)
    ("lblString"   ''label-text)
    ("portPos"     ''text)
    ("layerRange"  ''text)
    ("layerList"   ''text)
    ("outputMode"  ''(:breadthfirst :nodesfirst :edgesfirst))
    ("packMode"    ''text) ; "node", "clust" , "graph" , "array(_flags)?(%d)?"
    ("pagedir"     ''("BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT"))
    ("splineType"  ''text)
    ("quadType"    ''(:normal :fast :none))
    ("doubleList"  ''text)
    ("shape"       'cl-dot::*node-shapes*)

    ("rankType"    ''(:same :min :source :max :sink))
    ("rankdir"     ''("TB" "LR" "BT" "RL"))

    ("smoothType"  ''(:none :avg_dist :graph_dist :power_dist :rng :spring :triangle))
    ("startType"   ''text)
    ("style"       `(error "~@<~S should have been handled specially.~@:>"
                           ,string))
    ("pointList"   ''text)
    ("viewPort"    ''text)))

;;; Scraping of node, edge and cluster styles

(defun scrape-style (document context)
  (let* ((caption (ecase context
                    (:node    "Basic style settings for nodes")
                    (:edge    "Basic style settings for edges")
                    (:cluster "Basic style settings for clusters")))
         (path    (format nil "//table[caption/text()=~S]/tbody/tr/td/tt/text()"
                          caption)))
    (xloc:with-locations-r/o
        ((styles path :if-multiple-matches :all)
         :namespaces *xhtml-namespaces*)
        document
      styles)))

;;; Scraping of polygonal node shapes

(defun scrape-polygon-shapes (document)
  (xloc:with-locations-r/o
      ((shapes "//table[@align=\"CENTER\"]/tbody/tr/td/a/text()"
               :if-multiple-matches :all)
       :namespaces *xhtml-namespaces*)
      document
    shapes))

;;; Scraping of arrow shapes

(defun scrape-arrow-shapes (document)
  (xloc:with-locations-r/o
      ((shapes "//center[4]/table/tbody/tr/td[normalize-space(text())]/text()"
               :if-multiple-matches :all)
       :namespaces *xhtml-namespaces*)
      document
    (mapcar (curry #'string-trim *whitespace-characters*) shapes)))

;;; Emitter
;;;
;;; Write shape and style lists, write a form constructing an
;;; `attribute' instance for each scraped attribute.

(defun maybe-keywordify (string)
  (if (notany #'upper-case-p string)
      (make-keyword (string-upcase string))
      string))

(defun make-enum-definition (name values)
  `(defparameter ,name
     '(,@(mapcar #'maybe-keywordify values))))

(defun make-attributes-definition (attributes)
  `(defparameter cl-dot::*attributes*
     (list ,@(loop :for (name allowed-in type) :in attributes
                ;; Style attribute is context-dependent. Make one
                ;; `attribute' instance for each of the node, edge and
                ;; cluster contexts.
                :if (string= name "style")
                :collect `(cl-dot::make-attribute
                           ,name '(:node) cl-dot::*node-styles*)
                :and :collect `(cl-dot::make-attribute
                                ,name '(:edge) cl-dot::*edge-styles*)
                :and :collect `(cl-dot::make-attribute
                                ,name '(:cluster) cl-dot::*cluster-styles*)
                ;; Everything else should be fine
                :else
                :collect `(cl-dot::make-attribute ,name ',allowed-in ,type)))))

(defun write-attributes-definition ()
  (let* ((polygon-shapes      (scrape-polygon-shapes
                               (obtain-document *shapes-document-url*)))
         (arrow-shapes        (scrape-arrow-shapes
                               (obtain-document *arrows-document-url*)))
         (attributes-document (obtain-document *attribute-document-url*))
         (node-styles         (scrape-style attributes-document :node))
         (edge-styles         (scrape-style attributes-document :edge))
         (cluster-styles      (scrape-style attributes-document :cluster))
         (attributes          (scrape-attribute-definitions
                               attributes-document))
         (attributes          (sort attributes #'string-lessp :key #'first)))
    (with-output-to-file (stream *output-file* :if-exists :supersede)
      (with-standard-io-syntax
        (format stream ";;;;~{ ~<~%;;;; ~1,70:;~A~>~}~%"
                `("This" "file" "has" "been" "generated" "at"
                  ,@(nthcdr
                     3 (reverse
                        (multiple-value-list
                         (decode-universal-time (get-universal-time)))))
                  "from" ,*attribute-document-url* ","
                  ,*shapes-document-url* "and" ,*arrows-document-url* "."
                  "Do" "not" "modify" "by" "hand."))
        (let ((*print-case* :downcase))
          (let ((*package* (find-package '#:keyword)))
            (print '(in-package #:cl-dot) stream))
          (terpri stream)


          (let ((*package* (find-package '#:cl-dot)))
            (flet ((emit (form)
                     (pprint form stream)
                     (terpri stream)))
              ;; Node shapes
              (emit (make-enum-definition 'cl-dot::*node-shapes*
                                          (list* "record" polygon-shapes)))
              ;; Arrow shapes
              (emit (make-enum-definition 'cl-dot::*predefined-arrow-shapes*
                                          arrow-shapes))
              ;; Node styles
              (emit (make-enum-definition 'cl-dot::*node-styles* node-styles))
              ;; Edge styles
              (emit (make-enum-definition 'cl-dot::*edge-styles* edge-styles))
              ;; Cluster styles
              (emit (make-enum-definition 'cl-dot::*cluster-styles* cluster-styles))
              ;; Attributes
              (emit (make-attributes-definition attributes)))))))))
