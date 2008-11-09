(in-package cl-dot)

(defparameter *graph-attributes*
  '((:size text)
    (:page text)
    (:ratio (:fill :compress :auto)) ;; Could actually be a float number too
    (:margin float)
    (:nodesep float)
    (:node attribute)
    (:edge attribute)
    (:ranksep float)
    (:ordering (:out))
    (:rankdir ("LR" "RL" "BT"))
    (:pagedir text)
    (:rank (:same :min :max))
    (:rotate integer)
    (:center integer)
    (:nslimit float)
    (:mclimit float)
    (:layers text)
    (:color text)
    (:bgcolor text)))

(defparameter *node-attributes*
  '((:height integer)
    (:width integer)
    (:fixed-size boolean)
    (:label text)
    (:shape (:record :plaintext :ellipse :circle :egg :triangle :box
             :diamond :trapezium :parallelogram :house :hexagon :octagon))
    (:fontsize integer)
    (:fontname text)
    (:color text)
    (:fillcolor text)
    (:style (:filled :solid :dashed :dotted :bold :invis))
    (:layer text)))

(defparameter *edge-attributes*
  '((:minlen integer)
    (:weight integer)
    (:label text)
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
    (:headlabel text)
    (:taillabel text)
    (:labelfontsize integer)
    (:labelfontname text)
    (:labelfontcolor text)
    (:labeldistance integer)
    (:port-label-distance integer)
    (:decorate boolean)
    (:samehead boolean)
    (:sametail boolean)
    (:constraint boolean)
    (:layer text)))

