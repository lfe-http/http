(defmodule http
  (export
   (version 0) (versions 0)))

;;; -----------
;;; library API
;;; -----------

;;; Metadata

(defun version ()
  (http.vsn:get))

(defun versions ()
  (http.vsn:all))