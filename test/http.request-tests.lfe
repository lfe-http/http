(defmodule http.request-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest new
  (let ((req (http.request:new 'GET
                               "http://example.com/api/v1/thing?q=wut&flag=enabled")))
    (is-equal '(body
                headers
                method
                path-segments
                query-parsed
                url
                url-parsed
                version)
              (lists:sort (maps:keys req)))
    (is-equal '(1.1
                GET
                #M()
                #M(#"flag" #"enabled"
                   #"q" #"wut")
                #M(host #"example.com"
                        path #"/api/v1/thing"
                        query #"q=wut&flag=enabled"
                        scheme #"http")
                (#"api" #"v1" #"thing")
                #""
                #"http://example.com/api/v1/thing?q=wut&flag=enabled")
              (lists:sort (maps:values req)))))

(deftest new-with-body
  (let ((req (http.request:new 'POST
                               "http://example.com/api/v1/thing?q=wut&flag=enabled"
                               #"data...")))
    (is-equal '(body
                headers
                method
                path-segments
                query-parsed
                url
                url-parsed
                version)
              (lists:sort (maps:keys req)))
    (is-equal '(1.1
                POST
                #M()
                #M(#"flag" #"enabled"
                   #"q" #"wut")
                #M(host #"example.com"
                        path #"/api/v1/thing"
                        query #"q=wut&flag=enabled"
                        scheme #"http")
                (#"api" #"v1" #"thing")
                #"data..."
                #"http://example.com/api/v1/thing?q=wut&flag=enabled")
              (lists:sort (maps:values req)))))

(deftest new-with-headers
  (let* ((headers #m(#"Content-Type" #"application/json"))
         (req (http.request:new 'PUT
                               "http://example.com/api/v1/thing?q=wut&flag=enabled"
                               #"data..."
                               headers)))
    (is-equal '(body
                headers
                method
                path-segments
                query-parsed
                url
                url-parsed
                version)
              (lists:sort (maps:keys req)))
    (is-equal '(1.1
                PUT
                #M(#"Content-Type" #"application/json")
                #M(#"flag" #"enabled"
                   #"q" #"wut")
                #M(host #"example.com"
                        path #"/api/v1/thing"
                        query #"q=wut&flag=enabled"
                        scheme #"http")
                (#"api" #"v1" #"thing")
                #"data..."
                #"http://example.com/api/v1/thing?q=wut&flag=enabled")
              (lists:sort (maps:values req)))))