;;;; This module povides interoperability between the LFE HTP library and the
;;;; Erlang stdlib httpc HTTP client library.
(defmodule http.c
  (export
   (->erlang 1) (->erlang 3)
   (erlang-> 1)
   (request 1) (request 2) (request 3) (request 4) (request 5) (request 6)))

(defun request (url)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new url)))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun request (method url)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new method url)))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun request (method url body)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new method url body)))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun request (method url body headers)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new method url body headers)))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun request (method url body http-options options)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new method url body)
                http-options options))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun request (method url body headers http-options options)
  (case (apply #'httpc:request/4
               (->erlang
                (http.request:new method url body headers)
                http-options options))
    (`#(ok ,r) (erlang-> r))
    (err err)))

(defun ->erlang (req)
  "Convert an LFE HTTP library request to args that can be supplied to Erlang's
  `httpc:request/4` function."
  (->erlang
   req
   ;; default Erlang httpc `HttpOptions`
   `(#(version ,(http.util:http-version req)))
   ;; default Erlang httpc `Options`
   `(#(sync true) #(full_result true))))

(defun ->erlang (req http-options options)
  (->erlang (mref req 'method) req http-options options))

(defun erlang-> (httpc-resp)
  "Convert an Erlang HTTP client (httpc) response to an LFE HTTP library
  response."
  (let ((`#(,_ ,status-code ,_) (element 1 httpc-resp))
        (headers (http.header:list->map (element 2 httpc-resp)))
        (body (list_to_binary (element 3 httpc-resp))))
    (http.response:new status-code headers body)))

;; Private functions

(defun ->erlang
  ;; Without body
  (('delete req http-options options)
   (->erlang-no-body 'delete req http-options options))
  (('get req http-options options)
   (->erlang-no-body 'get req http-options options))
  (('head req http-options options)
   (->erlang-no-body 'head req http-options options))
  (('options req http-options options)
   (->erlang-no-body 'options req http-options options))
  (('trace req http-options options)
   (->erlang-no-body 'trace req http-options options))

  ;; With body
  (('patch req http-options options)
   (->erlang-with-body 'patch req http-options options))
  (('post req http-options options)
   (->erlang-with-body 'post req http-options options))
  (('put req http-options options)
   (->erlang-with-body 'put req http-options options)))

(defun ->erlang-no-body (method req http-options options)
  (let ((headers (mref req 'headers)))
    (list
     method
     `#(,(mref req 'url)
        ,(maps:to_list headers))
     http-options
     options)))

(defun ->erlang-with-body (method req http-options options)
  (let ((headers (mref req 'headers)))
    (list
     method
     `#(,(mref req 'url)
        ,(maps:to_list headers)
        ,(binary_to_list (maps:get #"Content-Type" headers (http.mimetype:text/html)))
        ,(mref req 'body))
     http-options
     (lists:append options '(#(body_format binary))))))
