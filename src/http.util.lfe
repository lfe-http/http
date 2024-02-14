(defmodule http.util
  (export
   (add-header 3)
   (http-version 1)))

(defun add-header (req-or-resp k v)
  (let* ((hs (mref req-or-resp 'headers))
         (hs (http.header:add hs k v)))
    (maps:put 'headers hs req-or-resp)))

(defun http-version (req)
  (io_lib:format "HTTP/~p" (list (mref req 'version))))
