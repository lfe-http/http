(defmodule http.util
  (export
   (http-version 1)))

(defun http-version (req)
  (io_lib:format "HTTP/~p" (list (mref req 'version))))
