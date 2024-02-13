(defmodule http.header
  (export
   (add 2) (add 3)
   (list->map 1)
   (new 0)))

(defun add (header-map kv)
  (let ((`#(,k ,v) (kv->bins kv)))
    (maps:put k v header-map)))

(defun add (header-map k v)
  (add header-map `#(,k ,v)))

(defun new ()
  `#m())

(defun list->map (proplist)
  (maps:from_list (list->bins proplist)))

(defun list->bins (proplist)
  (lists:map
   #'kv->bins/1
   proplist))

(defun kv->bins
  ((`#(,k ,v)) (when (andalso (is_binary k) (is_binary v)))
   `#(,k ,v))
  ((`#(,k ,v)) (when (is_list k))
   (kv->bins `#(,(list_to_binary k) ,v)))
  ((`#(,k ,v)) (when (is_list v))
   (kv->bins `#(,k ,(list_to_binary v))))
  ((`#(,k ,v)) (when (is_atom k))
   (kv->bins `#(,(atom_to_list k) ,v)))
  ((`#(,k ,v)) (when (is_atom v))
   (kv->bins `#(,k ,(atom_to_list v))))
  ((`#(,k ,v)) (when (is_integer v))
   (kv->bins `#(,k ,(integer_to_list v 10)))))
