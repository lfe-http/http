(defmodule http
  (export
   (default-headers 0)
   (default-version 0)
   (methods 0)))

;;; -----------
;;; library API
;;; -----------

(defun default-headers ()
  (http.header:new))

(defun default-version ()
  1.1)

(defun methods ()
  '(DELETE
    GET
    HEAD
    OPTIONS
    POST
    PUT
    TRACE))

(defun versions ()
  '(0.9
    1.0
    1.1
    2
    3))