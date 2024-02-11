(defmodule http.request
  (export
   (new 1) (new 2) (new 3) (new 4)))

(defun new
  ((url) (when (is_list url))
   (new (list_to_binary url)))
  ((url)
   `#m(method 'undefined
       version ,(http:default-version)
       remote-addr #"",
       headers ,(http:default-headers)
       body #""
       url ,url
       url-parsed ,(yuri:parse url)
       path-segments ()
       query-parsed ())))

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
