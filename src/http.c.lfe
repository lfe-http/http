;;;; This module povides interoperability between the LFE HTP library and the
;;;; Erlang stdlib httpc HTTP client library.
(defmodule http.c
  (export
    (request 1) (request 2) (request 3) (request 4)))

(defun request (url)
  (apply #'httpc:request/4 (http.request:->erlang (http.request:new url))))

(defun request (method url)
  (apply #'httpc:request/4 (http.request:->erlang (http.request:new method url))))

(defun request (method url body)
  (apply #'httpc:request/4 (http.request:->erlang (http.request:new method url body))))

(defun request (method url body headers)
  (apply #'httpc:request/4 (http.request:->erlang (http.request:new method url body headers))))
