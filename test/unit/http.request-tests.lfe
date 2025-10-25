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
