;;;; Request Builder Examples
;;;; Demonstrates the fluent builder pattern for constructing HTTP requests

(defmodule request-builder
  (export
   (simple-build 0)
   (query-params-example 0)
   (builder-chain-example 0)
   (content-type-helpers 0)))

(defun simple-build ()
  "Build a simple GET request"
  (let ((req (http.request:new #"GET" "https://api.example.com/users")))
    (io:format "Method: ~p~n" `(,(http.request:method req)))
    (io:format "URL: ~p~n" `(,(http.request:url req)))
    (io:format "Headers: ~p~n" `(,(http.request:headers req)))
    req))

(defun query-params-example ()
  "Build request with query parameters"
  ;; Start with base URL
  (let* ((req1 (http.request:new #"GET" "https://api.example.com/search"))
         ;; Add query parameters one by one
         (req2 (http.request:add-query-param req1 #"q" #"lisp"))
         (req3 (http.request:add-query-param req2 #"limit" #"10"))
         (req4 (http.request:add-query-param req3 #"offset" #"0")))

    (io:format "Final URL: ~p~n" `(,(http.request:url req4)))
    (io:format "Query params: ~p~n" `(,(http.request:query-params req4)))

    ;; Alternative: set all params at once
    (let* ((req-alt (http.request:new #"GET" "https://api.example.com/search"))
           (params #m(#"q" #"lisp" #"limit" #"10" #"offset" #"0"))
           (req-with-params (http.request:set-query-params req-alt params)))
      (io:format "Alternative URL: ~p~n" `(,(http.request:url req-with-params)))
      req-with-params)))

(defun builder-chain-example ()
  "Demonstrate fluent builder pattern chaining"
  ;; Build a complete POST request in a single let* expression
  (let* ((req1 (http.request:new #"POST" "https://api.example.com/articles"))
         (req2 (http.request:set-header req1 #"Authorization" #"Bearer token123"))
         (req3 (http.request:set-header req2 #"X-Request-ID" #"abc-123"))
         (req4 (http.request:set-json req3 #"{\"title\":\"LFE is awesome\"}"))
         (req5 (http.request:add-query-param req4 #"published" #"true")))

    (io:format "~nComplete Request:~n")
    (io:format "  Method: ~p~n" `(,(http.request:method req5)))
    (io:format "  URL: ~p~n" `(,(http.request:url req5)))
    (io:format "  Headers: ~p~n" `(,(http.request:headers req5)))
    (io:format "  Body: ~p~n" `(,(http.request:body req5)))
    req5))

(defun content-type-helpers ()
  "Demonstrate content-type helper functions"
  (io:format "~n=== JSON Content ===~n")
  (let* ((req1 (http.request:new #"POST" "https://api.example.com/data"))
         (json-data #"{\"key\":\"value\"}")
         (req-json (http.request:set-json req1 json-data)))
    (io:format "Body: ~p~n" `(,(http.request:body req-json)))
    (io:format "Content-Type: ~p~n"
               `(,(http.header:get (http.request:headers req-json)
                                   #"Content-Type"
                                   'undefined
                                   #m(case-insensitive true)))))

  (io:format "~n=== Form Content ===~n")
  (let* ((req2 (http.request:new #"POST" "https://api.example.com/form"))
         (form-data #m(#"field1" #"value1" #"field2" #"value2"))
         (req-form (http.request:set-form req2 form-data)))
    (io:format "Body: ~p~n" `(,(http.request:body req-form)))
    (io:format "Content-Type: ~p~n"
               `(,(http.header:get (http.request:headers req-form)
                                   #"Content-Type"
                                   'undefined
                                   #m(case-insensitive true)))))

  (io:format "~n=== Text Content ===~n")
  (let* ((req3 (http.request:new #"POST" "https://api.example.com/text"))
         (text-data #"This is plain text")
         (req-text (http.request:set-text req3 text-data)))
    (io:format "Body: ~p~n" `(,(http.request:body req-text)))
    (io:format "Content-Type: ~p~n"
               `(,(http.header:get (http.request:headers req-text)
                                   #"Content-Type"
                                   'undefined
                                   #m(case-insensitive true)))))
  'ok)

;;; Usage from LFE REPL:
;;;
;;; > (c "examples/request-builder.lfe")
;;; > (request-builder:simple-build)
;;; > (request-builder:query-params-example)
;;; > (request-builder:builder-chain-example)
;;; > (request-builder:content-type-helpers)
