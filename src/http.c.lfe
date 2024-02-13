;;;; This module povides interoperability between the LFE HTP library and the
;;;; Erlang stdlib httpc HTTP client library.
(defmodule http.c
  (export
    (request 1) (request 2) (request 3) (request 4) (request 5) (request 6)))

(defun request (url)
  (case (apply #'httpc:request/4
               (http.request:->erlang
                (http.request:new url)))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))

(defun request (method url)
  (case (apply #'httpc:request/4
               (http.request:->erlang
                (http.request:new method url)))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))

(defun request (method url body)
  (case (apply #'httpc:request/4
               (http.request:->erlang
                (http.request:new method url body)))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))

(defun request (method url body headers)
  (case (apply #'httpc:request/4
              (http.request:->erlang
               (http.request:new method url body headers)))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))

(defun request (method url body http-options options)
  (case (apply #'httpc:request/4
               (http.request:->erlang
                (http.request:new method url body)
                http-options options))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))

(defun request (method url body headers http-options options)
  (case (apply #'httpc:request/4
               (http.request:->erlang
                (http.request:new method url body headers)
                http-options options))
    (`#(ok ,r) (http.response:erlang-> r))
    (err err)))
