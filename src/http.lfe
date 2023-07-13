(defmodule http
  (export
   (methods 0)))

;;; -----------
;;; library API
;;; -----------

(defun methods ()
  '(DELETE
    GET
    HEAD
    OPTIONS
    POST
    PUT
    TRACE))
