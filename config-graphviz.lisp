(in-package :cl-dot)

;;; I'd much rather look for "dot" and "neato" in environment variables, and
;;; then the user's path first, but that seems beyond the bounds of portability
;;; [2014/03/15:rpg]

(defun find-dot ()
  "Find the DOT program using either the environment variable CL_DOT_DOT, search in the user's
path, or search of likely installation locations."
  (or
   (uiop:getenv "CL_DOT_DOT")
   (check-in-path "dot")
   (loop for file in #+(or win32 mswindows) (list "\"C:/Program Files/ATT/Graphviz/bin/dot.exe\"")
         #-(or win32 mswindows) (list "/usr/local/bin/dot" "/opt/local/bin/dot" "/usr/bin/dot")
         when (probe-file file)
           return file
         finally (return nil))))

(defun find-neato ()
  "Find the NEATO program using either the environment variable CL_DOT_NEATO, search in the user's
path, or search of likely installation locations."
  (or
   (uiop:getenv "CL_DOT_NEATO")
   (check-in-path "neato")
   (loop for file in #+(or win32 mswindows) (list "\"C:/Program Files/ATT/Graphviz/bin/neato.exe\"")
         #-(or win32 mswindows) (list "/usr/local/bin/neato" "/opt/local/bin/neato" "/usr/bin/neato")
         when (probe-file file)
           return file
         finally (return nil))))


(defun check-in-path (name)
  (unless (uiop:os-windows-p)
    (multiple-value-bind (outstring errstring exit-code)
        (uiop:run-program (format nil "which ~a" name) :force-shell t :output '(:string :stripped t) :ignore-error-status t)
      (declare (ignore errstring))
      (when (zerop exit-code) outstring))))
