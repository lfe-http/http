(defmodule http.request-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest new-1
  (let ((req (http.request:new "http://example.com/")))
    (is-equal #"GET" (mref req 'method))
    (is-equal #"http://example.com/" (mref req 'url))
    (is-equal #"" (mref req 'body))
    (is-equal #m() (mref req 'headers))
    (is-equal 1.1 (mref req 'version))))

(deftest new-2
  (let ((req (http.request:new 'post "http://example.com/")))
    (is-equal #"POST" (mref req 'method))
    (is-equal #"http://example.com/" (mref req 'url))
    (is-equal #"" (mref req 'body))))

(deftest new-3
  (let ((req (http.request:new 'post "http://example.com/" #"stuff")))
    (is-equal #"POST" (mref req 'method))
    (is-equal #"stuff" (mref req 'body))))

(deftest new-4
  (let ((req (http.request:new 'post
                                "http://example.com/"
                                #"stuff"
                                #m(content-type #"application/json"))))
    (is-equal #"POST" (mref req 'method))
    (is-equal #"stuff" (mref req 'body))
    (is-equal #m(content-type #"application/json") (mref req 'headers))))

(deftest new
  (let ((req (maps:merge
               (http.request:new 'put
                                 "http://alice.roberts:sekr1t@example.com:5099/api/v1/thing?q=wut&flag=enabled#start")
               #m(remote-addr #"172.16.32.42"))))
    (is-equal #"PUT" (mref req 'method))
    (is-equal #"" (mref req 'body))
    (is-equal #m() (mref req 'headers))
    (is-equal '(#"api" #"v1" #"thing") (mref req 'path-segments))
    (is-equal #"172.16.32.42" (mref req 'remote-addr))
    ;; Check that query params are present
    (is-equal #"enabled" (maps:get #"flag" (mref req 'query-parsed)))
    (is-equal #"wut" (maps:get #"q" (mref req 'query-parsed)))))

;;; ---------------------------------------------------------------------------
;;; Setter Function Tests
;;; ---------------------------------------------------------------------------

(deftest set-method
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-method req #"POST")))
    (is-equal #"POST" (mref req2 'method))))

(deftest set-method-from-atom
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-method req 'delete)))
    (is-equal #"DELETE" (mref req2 'method))))

(deftest set-body
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-body req #"test data")))
    (is-equal #"test data" (mref req2 'body))))

(deftest set-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-header req #"Content-Type" #"application/json")))
    (is-equal #"application/json"
              (http.header:get (mref req2 'headers) #"Content-Type" 'undefined #m(case-insensitive true)))))

(deftest add-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:add-header req #"X-Custom" #"value1"))
         (req3 (http.request:add-header req2 #"X-Other" #"value2")))
    ;; Test both headers are present
    (is-equal #"value1" (http.header:get (mref req3 'headers) #"X-Custom" 'undefined #m(case-insensitive true)))
    (is-equal #"value2" (http.header:get (mref req3 'headers) #"X-Other" 'undefined #m(case-insensitive true)))))

(deftest remove-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-header req #"X-Custom" #"value"))
         (req3 (http.request:remove-header req2 #"X-Custom")))
    (is-equal 'false (http.header:has-key? (mref req3 'headers) #"X-Custom" #m(case-insensitive true)))))

(deftest set-headers
  (let* ((req (http.request:new "http://example.com/"))
         (headers #m(#"Content-Type" #"application/json"
                     #"Accept" #"application/json"))
         (req2 (http.request:set-headers req headers)))
    (is-equal headers (mref req2 'headers))))

;;; ---------------------------------------------------------------------------
;;; Body Helper Function Tests
;;; ---------------------------------------------------------------------------

(deftest set-json
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-json req #"{\"key\":\"value\"}")))
    (is-equal #"{\"key\":\"value\"}" (mref req2 'body))
    (is-equal #"application/json; charset=utf-8"
              (http.header:get (mref req2 'headers) #"Content-Type" 'undefined #m(case-insensitive true)))))

(deftest set-form-binary
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-form req #"key=value&foo=bar")))
    (is-equal #"key=value&foo=bar" (mref req2 'body))
    (is-equal #"application/x-www-form-urlencoded"
              (http.header:get (mref req2 'headers) #"Content-Type" 'undefined #m(case-insensitive true)))))

(deftest set-form-map
  (let* ((req (http.request:new "http://example.com/"))
         (params #m(#"key" #"value" #"foo" #"bar"))
         (req2 (http.request:set-form req params))
         (body (mref req2 'body)))
    ;; Body should contain both params (order may vary)
    (is-not-equal 'nomatch (binary:match body #"key=value"))
    (is-not-equal 'nomatch (binary:match body #"foo=bar"))
    (is-equal #"application/x-www-form-urlencoded"
              (http.header:get (mref req2 'headers) #"Content-Type" 'undefined #m(case-insensitive true)))))

(deftest set-text
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-text req #"plain text")))
    (is-equal #"plain text" (mref req2 'body))
    (is-equal #"text/plain; charset=utf-8"
              (http.header:get (mref req2 'headers) #"Content-Type" 'undefined #m(case-insensitive true)))))

;;; ---------------------------------------------------------------------------
;;; Query Parameter Function Tests
;;; ---------------------------------------------------------------------------

(deftest add-query-param
  (let* ((req (http.request:new "http://example.com/path"))
         (req2 (http.request:add-query-param req #"key1" #"value1"))
         (req3 (http.request:add-query-param req2 #"key2" #"value2")))
    (is-equal #"value1" (maps:get #"key1" (mref req3 'query-parsed)))
    (is-equal #"value2" (maps:get #"key2" (mref req3 'query-parsed)))
    ;; URL should be updated with query string
    (is-not-equal 'nomatch (binary:match (mref req3 'url) #"key1=value1"))
    (is-not-equal 'nomatch (binary:match (mref req3 'url) #"key2=value2"))))

(deftest add-query-param-to-existing
  (let* ((req (http.request:new "http://example.com/path?existing=param"))
         (req2 (http.request:add-query-param req #"new" #"value")))
    (is-equal #"param" (maps:get #"existing" (mref req2 'query-parsed)))
    (is-equal #"value" (maps:get #"new" (mref req2 'query-parsed)))))

(deftest set-query-params
  (let* ((req (http.request:new "http://example.com/path?old=param"))
         (params #m(#"key1" #"value1" #"key2" #"value2"))
         (req2 (http.request:set-query-params req params)))
    (is-equal params (mref req2 'query-parsed))
    ;; Old param should be gone
    (is-equal 'false (maps:is_key #"old" (mref req2 'query-parsed)))))

(deftest set-query-params-empty
  (let* ((req (http.request:new "http://example.com/path?old=param"))
         (req2 (http.request:set-query-params req #m())))
    (is-equal #m() (mref req2 'query-parsed))
    ;; URL should not have query string
    (is-equal #"http://example.com/path" (mref req2 'url))))

;;; ---------------------------------------------------------------------------
;;; Getter Function Tests
;;; ---------------------------------------------------------------------------

(deftest method-getter
  (let ((req (http.request:new #"POST" "http://example.com/")))
    (is-equal #"POST" (http.request:method req))))

(deftest url-getter
  (let ((req (http.request:new "http://example.com/path")))
    (is-equal #"http://example.com/path" (http.request:url req))))

(deftest body-getter
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-body req #"test")))
    (is-equal #"test" (http.request:body req2))))

(deftest headers-getter
  (let* ((req (http.request:new "http://example.com/"))
         (headers #m(#"X-Custom" #"value"))
         (req2 (http.request:set-headers req headers)))
    (is-equal headers (http.request:headers req2))))

(deftest path-segments-getter
  (let ((req (http.request:new "http://example.com/api/v1/users")))
    (is-equal '(#"api" #"v1" #"users") (http.request:path-segments req))))

(deftest path-segments-getter-root
  (let ((req (http.request:new "http://example.com/")))
    (is-equal '() (http.request:path-segments req))))

(deftest query-params-getter
  (let ((req (http.request:new "http://example.com/?key=value&foo=bar")))
    (is-equal #"value" (maps:get #"key" (http.request:query-params req)))
    (is-equal #"bar" (maps:get #"foo" (http.request:query-params req)))))

(deftest query-params-getter-empty
  (let ((req (http.request:new "http://example.com/")))
    (is-equal #m() (http.request:query-params req))))
