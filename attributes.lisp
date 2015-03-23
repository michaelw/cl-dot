;; See http://www.graphviz.org/doc/info/attrs.html

(in-package cl-dot)

(defparameter *graph-attributes*
  '((:label label-text)
    (:size text)
    (:page text)
    (:ratio (:fill :compress :auto)) ;; Could actually be a float number too
    (:margin float)
    (:nodesep float)
    (:node attribute)
    (:edge attribute)
    (:ranksep float)
    (:ordering (:out))
    (:rankdir ("LR" "RL" "BT"))
    (:dir ("forward" "none"))
    (:pagedir text)
    (:layout text) ;; neato, twopi, etc.
    (:rank (:same :min :max))
    (:rotate integer)
    (:center integer)
    (:nslimit float)
    (:mclimit float)
    (:layers text)
    (:fontsize integer)
    (:fontname text)
    (:color text)
    (:bgcolor text)
    (:url text)
    (:target text)
    (:stylesheet text)))

(defparameter *node-attributes*
  '((:height integer)
    (:width integer)
    (:fixed-size boolean)
    (:label label-text)
    (:shape (:record :plaintext :ellipse :circle :egg :triangle :box
             :diamond :trapezium :parallelogram :house :hexagon :octagon
             :doubleoctagon))
    (:fontsize integer)
    (:fontname text)
    (:color text)
    (:fillcolor text)
    (:style (:filled :solid :dashed :dotted :bold :invis))
    (:layer text)
    (:url text)
    (:target text)))

(defparameter *edge-attributes*
  '((:minlen integer)
    (:weight integer)
    (:arrowhead ("normal" "inv" "dot" "invdot" "odot" "invodot" "none" "tee" "empty" "invempty" "diamond" "odiamond" "ediamond" "crow" "box" "obox" "open" "halfopen" "vee" "circle"))
    (:label label-text)
    (:fontsize integer)
    (:fontname text)
    (:fontcolor text)
    (:style (:solid :dashed :dotted :bold :invis))
    (:color text)
    (:dir (:forward :back :both :none))
    (:tailclip boolean)
    (:headclip boolean)
    (:arrowhead (:none :normal :inv :dot :odot :invdot :invodot :tee
                 :empty :invempty :open :halfopen :diamond :odiamond
                 :box :obox :crow))
    (:arrowtail (:none :normal :inv :dot :odot :invdot :invodot :tee
                 :empty :invempty :open :halfopen :diamond :odiamond
                 :box :obox :crow))
    (:headlabel label-text)
    (:taillabel label-text)
    (:labelfontsize integer)
    (:labelfontname text)
    (:labelfontcolor text)
    (:labeldistance integer)
    (:port-label-distance integer)
    (:decorate boolean)
    (:samehead boolean)
    (:sametail boolean)
    (:constraint boolean)
    (:layer text)
    (:url text)
    (:target text)))
