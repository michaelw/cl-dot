(in-package cl-dot)

(defvar *dot-path*
  #+(or win32 mswindows) "\"C:/Program Files/ATT/Graphviz/bin/dot.exe\""
  #-(or win32 mswindows) "/usr/bin/dot"
  "Path to the dot command")

;; the path to the neato executable (used for drawing undirected
;; graphs).
(defvar *neato-path*
  #+(or win32 mswindows) "\"C:/Program Files/ATT/Graphviz/bin/neato.exe\""
  #-(or win32 mswindows) "/usr/bin/neato"
  "Path to the neato command")

(eval-when (:load-toplevel :execute)
  (setf *dot-path* (find-dot))
  (setf *neato-path* (find-neato)))

;;; Classes

(defvar *id*)

(defclass id-mixin ()
  ((id :initform (incf *id*) :initarg :id :accessor id-of)))

(defclass attributes-mixin ()
  ((attributes :initform nil :initarg :attributes :accessor attributes-of)))

(defclass graph (attributes-mixin)
  ((nodes :initform nil :initarg :nodes :accessor nodes-of)
   (edges :initform nil :initarg :edges :accessor edges-of)))

(defclass node (id-mixin
                attributes-mixin)
  ()
  (:documentation "A graph node with `dot` attributes (a plist, initarg
:ATTRIBUTES) and an optional `dot` id (initarg :ID, autogenerated
by default)."))

(defclass port-mixin ()
  ((source-port :initform nil :initarg :source-port :accessor source-port-of)
   (target-port :initform nil :initarg :target-port :accessor target-port-of)))

(defclass attributed (attributes-mixin
                      port-mixin)
  ((object :initarg :object :accessor object-of))
  (:documentation "Wraps an object (initarg :OBJECT) with `dot` attribute
information (a plist, initarg :ATTRIBUTES)"))

(defmethod print-object ((object attributed) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (object-of object))))

(defclass edge (attributes-mixin
                port-mixin)
  ((source :initform nil :initarg :source :accessor source-of)
   (target :initform nil :initarg :target :accessor target-of)))

;;; Protocol functions

(defgeneric graph-object-node (graph object)
  (:documentation
   "Returns a NODE instance for this object, or NIL.

In the latter case the object will not be included in the graph, but
it can still have an indirect effect via other protocol
functions (e.g. GRAPH-OBJECT-KNOWS-OF).  This function will only be
called once for each object during the generation of a graph.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-node object)))

(defgeneric graph-object-edges (graph)
  (:documentation
   "Returns a sequence of edge specifications.

An edge specification is a list (FROM TO [ATTRIBUTES]), where FROM and
TO are objects of the graph and optional ATTRIBUTES is a plist of edge
attributes.")
  (:method (graph)
    (declare (ignore graph))
    '()))

(defgeneric graph-object-points-to (graph object)
  (:documentation
   "Returns a sequence of objects to which the NODE of this object
should be connected.

The edges will be directed from this object to the others.  To assign
dot attributes to the generated edges, each object can optionally be
wrapped in a instance of ATTRIBUTED.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-points-to object))
  (:method (graph (object t))
    '()))

(defgeneric graph-object-pointed-to-by (graph object)
  (:documentation
   "Returns a sequence of objects to which the NODE of this object
should be connected.

The edges will be directed from the other objects to this one. To
assign dot attributes to the generated edges, each object can
optionally be wrapped in a instance of ATTRIBUTED.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-pointed-to-by object))
  (:method (graph (object t))
    '()))

(defgeneric graph-object-knows-of (graph object)
  (:documentation
   "Returns a sequence of objects that this object knows should be
part of the graph, but which it has no direct connections to.")
  (:method ((graph (eql 'default)) object)
    (declare (ignore graph))
    (object-knows-of object))
  (:method (graph (object t))
    '()))

;;; Public interface

(defgeneric generate-graph-from-roots (graph objects &optional attributes)
  (:documentation "Constructs a GRAPH with ATTRIBUTES starting
from OBJECTS, using the GRAPH-OBJECT- protocol.")
  (:method (graph objects &optional attributes)
    (multiple-value-bind (nodes edges)
        (construct-graph graph objects)
      (make-instance 'graph
                     :attributes attributes
                     :nodes nodes
                     :edges edges))))

(defun print-graph (graph &rest options
                    &key (stream *standard-output*) (directed t))
  "Prints a dot-format representation GRAPH to STREAM."
  (declare (ignore stream directed))
  (apply #'generate-dot
         (nodes-of graph)
         (edges-of graph)
         (attributes-of graph)
         options))

(defun dot-graph (graph outfile &key (format :ps) (directed t))
  "Renders GRAPH to OUTFILE by running the program in \*DOT-PATH* or
*NEATO-PATH* depending on the value of the DIRECTED keyword
argument.  The default is a directed graph.  The default
FORMAT is Postscript."
  (when (null format) (setf format :ps))

  (let ((dot-path (if directed *dot-path* *neato-path*))
        (format (format nil "-T~(~a~)" format))
        (dot-string (with-output-to-string (stream)
                      (print-graph graph
                                   :stream stream
                                   :directed directed))))
    (unless dot-path
      (error "neither 'dot' or 'neato' binary are found.
Consider something like sudo apt install graphviz!"))
    (uiop:run-program (list dot-path format "-o" (namestring outfile))
                      :input (make-string-input-stream dot-string)
                      :output *standard-output*)))

;;; Internal
(defun construct-graph (graph objects)
  (let ((handled-objects (make-hash-table))
        (nodes nil)
        (edges nil)
        (*id* 0))
    (labels ((add-edge (source target attributes &optional source-port target-port)
               (let ((edge (make-instance 'edge
                                          :attributes attributes
                                          :source source
                                          :source-port source-port
                                          :target target
                                          :target-port target-port)))
                 (push edge edges)))
             (get-node (object)
               (if (typep object 'attributed)
                   (multiple-value-call #'values
                     (get-node (object-of object))
                     (source-port-of object)
                     (target-port-of object))
                   (gethash object handled-objects)))
             (get-attributes (object)
               (when (typep object 'attributed)
                 (attributes-of object)))
             (handle-object (object)
               (when (typep object 'attributed)
                 (return-from handle-object
                   (handle-object (object-of object))))
               ;; If object has been already been visited, skip
               (unless (nth-value 1 (get-node object))
                 (let ((node (graph-object-node graph object))
                       (knows-of (graph-object-knows-of graph object))
                       (points-to (graph-object-points-to graph object))
                       (pointed-to (graph-object-pointed-to-by graph object)))
                   (setf (gethash object handled-objects) node)
                   (map nil #'handle-object knows-of)
                   (map nil #'handle-object points-to)
                   (map nil #'handle-object pointed-to)
                   (when node
                     (push node nodes)
                     (map nil
                          (lambda (to)
                            (multiple-value-bind (target found? source-port target-port)
                                (get-node to)
                              (when found?
                                (add-edge node target (get-attributes to)
                                          source-port target-port))))
                          points-to)
                     (map nil
                          (lambda (from)
                            (multiple-value-bind (source found? source-port target-port)
                                (get-node from)
                              (when found?
                                (add-edge source node (get-attributes from)
                                          source-port target-port))))
                          pointed-to)))))
             (handle-edge (from to &optional attributes)
               (handle-object from)
               (handle-object to)
               (let ((source (get-node from))
                     (target (get-node to)))
                 (add-edge source target attributes))))
      (map nil #'handle-object objects)
      (map nil
           (lambda (edge-spec)
             (apply #'handle-edge edge-spec))
           (graph-object-edges graph))
      (values nodes edges))))

(defun generate-dot (nodes edges attributes
                     &key (stream *standard-output*) (directed t))
  (with-standard-io-syntax ()
    (let ((*standard-output* (or stream *standard-output*))
          (*print-right-margin* 65535)
          (edge-op (if directed "->" "--"))
          (graph-type (if directed "digraph" "graph"))
          (node-defaults '())
          (edge-defaults '()))
      (format stream "~a {~%" graph-type)
      (loop for (name value) on attributes by #'cddr do
           (case name
             (:node
              (setf node-defaults (append node-defaults value)))
             (:edge
              (setf edge-defaults (append edge-defaults value)))
             (t
              (print-key-value stream name value *graph-attributes*)
              (format stream ";~%"))))
      ;; Default attributes.
      (print-defaults stream "node" node-defaults *node-attributes*)
      (print-defaults stream "edge" edge-defaults *edge-attributes*)
      ;; Nodes and edges.
      (dolist (node nodes)
        (format stream "  ~a " (textify (id-of node)))
        (print-attributes stream (attributes-of node) *node-attributes*)
        (format stream ";~%"))
      (dolist (edge edges)
        (format stream "  ~a~@[:~a~] ~a ~a~@[:~a~]"
                (textify (id-of (source-of edge))) (source-port-of edge)
                edge-op
                (textify (id-of (target-of edge))) (target-port-of edge))
        (print-attributes stream (attributes-of edge) *edge-attributes*)
        (format stream ";~%"))
      (format stream "}")
      (values))))

(defun print-defaults (stream kind attributes schema)
  (when attributes
    (format stream "  ~A " kind)
    (print-attributes stream attributes schema)
    (format stream "~%")))

(defun print-attributes (stream attributes schema)
  (format stream "[")
  (loop for (name value) on attributes by #'cddr
     for prefix = "" then "," do
       (write-string prefix)
       (print-key-value stream name value schema))
  (format stream "]"))

(defun print-key-value (stream key value attributes)
  (let* ((attribute    (find-attribute key attributes))
         (foreign-name (attribute-foreign-name attribute))
         (type         (attribute-type attribute)))
    (flet ((text-value (value)
             (typecase value
               (cons
                (destructuring-bind (alignment value) value
                  (textify value :alignment alignment)))
               (t
                (textify value)))))
      (format stream "~a=~a" foreign-name
              (etypecase type
                ((member integer)
                 (unless (typep value 'integer)
                   (error "Invalid value for ~S: ~S is not an integer"
                          key value))
                 value)
                ((member boolean)
                 (if value
                     "true"
                     "false"))
                ((member label-text)
                 (typecase value
                   ((cons (eql :html))
                    (htmlify value))
                   (t
                    (text-value value))))
                ((member text)
                 (text-value value))
                ((member float)
                 (coerce value 'single-float))
                (list
                 (flet ((stringify (value)
                          (unless (member value type :test 'equal)
                            (error "Invalid value for ~S: ~S is not one of ~S"
                                   key value type))
                          (if (symbolp value)
                              (string-downcase value)
                              value)))
                   (if (listp value)
                       (format nil "\"~{~A~^,~}\"" (mapcar #'stringify value))
                       (stringify value)))))))))

(defun htmlify (object)
  (check-type object (cons (eql :html) (cons null)))
  (with-output-to-string (stream)
    (labels
        ((escape-string (string &optional (stream stream))
           (loop :for c :across string :do
              (case c
                (#\"
                 (write-string "&quot;" stream))
                (#\<
                 (write-string "&lt;" stream))
                (#\>
                 (write-string "&gt;" stream))
                (#\&
                 (write-string "&amp;" stream))
                (#\Newline
                 (write-string "<br/>" stream))
                (t
                 (write-char c stream)))))
         (escape-attribute (attribute)
           (list
            (first attribute)
            (with-output-to-string (stream)
              (escape-string (second attribute) stream))))
         (textify-node (node)
           (etypecase node
             (cons
              (destructuring-bind (name attributes &rest children) node
                (format stream "<~A~@[ ~{~{~A=\"~A\"~}~^ ~}~]>"
                        name (mapcar #'escape-attribute attributes))
                (mapc #'textify-node children)
                (format stream "</~A>" name)))
             (string
              (escape-string node)))))
      (write-char #\< stream)
      (mapc #'textify-node (nthcdr 2 object))
      (write-char #\> stream))))

(defun textify (object &key alignment)
  (check-type alignment (member nil :center :left :right))
  (let ((string (princ-to-string object))
        (alignment (or alignment :center)))
    (with-output-to-string (stream)
      (write-char #\" stream)
      (loop for c across string do
            ;; Note: #\\ should not be escaped to allow \n, \l, \N, etc.
            ;; to work.
            (case c
              ((#\")
               (write-char #\\ stream)
               (write-char c stream))
              (#\Newline
               (write-char #\\ stream)
               (ecase alignment
                 (:center
                  (write-char #\n stream))
                 (:left
                  (write-char #\l stream))
                 (:right
                  (write-char #\r stream))))
              (t
               (write-char c stream))))
      (write-char #\" stream))))
