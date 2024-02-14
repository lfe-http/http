(defmodule http.request
  (export
   (->list 1)
   (add-header 3)
   (new 1) (new 2) (new 3) (new 4)))

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

(defun add-header (req k v)
  (http.util:add-header req k v))
