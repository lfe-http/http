(defmodule http.response
  (export
   (add-header 3)
   (new 0) (new 1) (new 2) (new 3)
   (set-body 2)))

(defun new ()
  (new #""))

(defun new (body)
  (new (http.status:ok) body))

(defun new (status body)
  (new status (http:default-headers) body))

(defun new (status headers body)
  `#m(status ,status
      headers ,headers
      body ,body))

(defun add-header (resp k v)
  (http.util:add-header resp k v))

(defun set-body
  ((resp v) (when (is_binary v))
   (mset resp 'body v))
  ((resp v) (when (is_list v))
   (set-body resp (list_to_binary v)))
  ((resp v) (when (is_atom v))
   ;; Once the minimum version of Erlang is 24, we can use this instead:
   ;;(set-body resp (atom_to_binary v)))
   (set-body resp (list_to_binary (atom_to_list v))))
  ((resp v) (when (is_integer v))
   (set-body resp (integer_to_binary v))))
