(defmodule http.c-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; Erlang Conversion Tests
;;; ---------------------------------------------------------------------------

(deftest erlang-conversion-get
  (let* ((req (http.request:new #"GET" #"http://example.com"))
         (args (http.c:->erlang req))
         (`(,method ,request ,_http-opts ,_opts) args)
         (`#(,url ,_headers) request))
    (is-equal 'get method)
    (is-equal #"http://example.com" url)))

(deftest erlang-conversion-post
  (let* ((req (http.request:new #"POST" #"http://example.com" #"body"))
         (args (http.c:->erlang req))
         (`(,method ,request ,_http-opts ,_opts) args)
         (`#(,url ,_headers ,_content-type ,body) request))
    (is-equal 'post method)
    (is-equal #"http://example.com" url)
    (is-equal #"body" body)))

;;; ---------------------------------------------------------------------------
;;; Method Dispatch Tests
;;; ---------------------------------------------------------------------------

(deftest method-dispatch-get
  (let* ((req (http.request:new #"GET" #"http://example.com"))
         (`(,method #(,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'get method)))

(deftest method-dispatch-head
  (let* ((req (http.request:new #"HEAD" #"http://example.com"))
         (`(,method #(,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'head method)))

(deftest method-dispatch-delete
  (let* ((req (http.request:new #"DELETE" #"http://example.com"))
         (`(,method #(,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'delete method)))

(deftest method-dispatch-post
  (let* ((req (http.request:new #"POST" #"http://example.com" #"body"))
         (`(,method #(,_ ,_ ,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'post method)))

(deftest method-dispatch-put
  (let* ((req (http.request:new #"PUT" #"http://example.com" #"body"))
         (`(,method #(,_ ,_ ,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'put method)))

(deftest method-dispatch-patch
  (let* ((req (http.request:new #"PATCH" #"http://example.com" #"body"))
         (`(,method #(,_ ,_ ,_ ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'patch method)))

;;; ---------------------------------------------------------------------------
;;; Headers Conversion Tests
;;; ---------------------------------------------------------------------------

(deftest headers-conversion
  (let* ((headers #m(#"Content-Type" #"application/json"
                     #"X-Custom" #"value"))
         (req (http.request:new #"GET" #"http://example.com" #"" headers))
         (`(,_ #(,_ ,header-list) ,_ ,_) (http.c:->erlang req)))
    (is-equal 'true (is_list header-list))
    (is-equal 2 (length header-list))))

;;; ---------------------------------------------------------------------------
;;; Response Conversion Tests
;;; ---------------------------------------------------------------------------

(deftest erlang-response-conversion
  (let* ((httpc-resp `#(#(#(1 1) 200 #"OK")
                        (#(#"content-type" "text/html")
                         #(#"content-length" "123"))
                        #"response body"))
         (lfe-resp (http.c:erlang-> httpc-resp)))
    (is-equal 200 (mref lfe-resp 'status))
    (is-equal #"response body" (mref lfe-resp 'body))
    (is-equal 'true (is_map (mref lfe-resp 'headers)))
    (is-equal 1.1 (mref lfe-resp 'version))))

(deftest erlang-response-headers
  (let* ((httpc-resp `#(#(#(1 1) 200 #"OK")
                        (#(#"content-type" "text/html")
                         #(#"content-length" "123"))
                        #"body"))
         (lfe-resp (http.c:erlang-> httpc-resp))
         (headers (mref lfe-resp 'headers)))
    (is-equal #"text/html"
              (http.header:get headers #"Content-Type" 'undefined #m(case-insensitive true)))
    (is-equal #"123"
              (http.header:get headers #"Content-Length" 'undefined #m(case-insensitive true)))))

;;; ---------------------------------------------------------------------------
;;; Body Format Tests
;;; ---------------------------------------------------------------------------

(deftest body-format-option
  (let* ((req (http.request:new #"POST" #"http://example.com" #"body"))
         (`(,_ ,_ ,_ ,opts) (http.c:->erlang req)))
    (is-equal 'true (lists:member #(body_format binary) opts))))

(deftest no-body-format-option
  (let* ((req (http.request:new #"GET" #"http://example.com"))
         (`(,_ ,_ ,_ ,opts) (http.c:->erlang req)))
    (is-equal 'false (lists:member #(body_format binary) opts))))

;;; ---------------------------------------------------------------------------
;;; Content-Type Tests
;;; ---------------------------------------------------------------------------

(deftest content-type-extraction
  (let* ((headers #m(#"Content-Type" #"application/json"))
         (req (http.request:new #"POST" #"http://example.com" #"body" headers))
         (`(,_ #(,_ ,_ ,content-type ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal "application/json" content-type)))

(deftest default-content-type
  (let* ((req (http.request:new #"POST" #"http://example.com" #"body"))
         (`(,_ #(,_ ,_ ,content-type ,_) ,_ ,_) (http.c:->erlang req)))
    (is-equal "application/octet-stream" content-type)))
