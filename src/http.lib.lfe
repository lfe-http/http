(defmodule http.lib
  (export
   (version 0) (versions 0)))

;;; -----------
;;; library metadata
;;; -----------

(defun version ()
  (http.vsn:get))

(defun versions ()
  (http.vsn:all))