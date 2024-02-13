(defmodule http.response
  (export
   (new 0) (new 1) (new 2) (new 3)))

(defun new ()
  (new #""))

(defun new (body)
  (new (http.status:ok) body))

(defun new (status body)
  (new status '() body))

(defun new (status headers body)
  `#m(status ,status
      headers ,headers
      body ,body))
