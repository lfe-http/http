(defmodule http
  (export
   (my-fun 0)))

;;; -----------
;;; library API
;;; -----------

(defun my-fun ()
  'hello-world)

;;; Metadata

(defun version ()
  (http.vsn:get))

(defun versions ()
  (http.vsn:all))