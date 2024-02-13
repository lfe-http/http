(defmodule http.request
  (export
   (new 1) (new 2) (new 3) (new 4)
   (->list 1)
   (->erlang 1) (->erlang 3)))

(defun new
  ((url) (when (is_list url))
   (new (list_to_binary url)))
  ((url)
   (let ((parsed-url (yuri:parse url)))
     `#m(method get
         version ,(http:default-version)
         remote-addr #""
         headers ,(http:default-headers)
         body #""
         url ,url
         url-parsed ,(maps:merge parsed-url (yuri.user:parse parsed-url))
         path-segments ()
         query-parsed #m()))))

(defun new (method url)
  (new method url #""))

(defun new (method url body)
  (new method url body (http:default-headers)))

(defun new (method url body headers)
  (let* ((init (new url))
         (url (mref init 'url-parsed)))
    (maps:merge init
                `#m(method ,method
                    body ,body
                    headers ,headers
                    path-segments ,(yuri.path:->segments (mref url 'path))
                    query-parsed ,(yuri.query:parse (mref url 'query))))))

(defun ->list (req)
  (let* ((h (maps:get 'headers req #m()))
         (q (maps:get 'query-parsed req #m()))
         (u (maps:get 'url-parsed req #m()))
         (req2 (maps:merge req `#m(headers ,(lists:sort (maps:to_list h))
                                   query-parsed ,(lists:sort (maps:to_list q))
                                   url-parsed ,(lists:sort (maps:to_list u))))))
    (lists:sort (maps:to_list req2))))

(defun ->erlang (req)
  "Convert an LFE HTTP library request to args that can be supplied to Erlang's
  `httpc:request/4` function.

  `options` is "
  (->erlang
    req
    `(#(version ,(http-version req)))   ; default `HttpOptions`
    `(#(sync true) #(full_result true)) ; default `Options`
    ))

(defun ->erlang (req http-options options)
  (->erlang (mref req 'method) req http-options options))

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

(defun http-version (req)
  (io_lib:format "HTTP/~p" (list (mref req 'version))))
