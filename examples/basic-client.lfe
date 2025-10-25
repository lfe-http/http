;;;; Basic HTTP Client Examples
;;;; Demonstrates simple HTTP client operations using http.c

(defmodule basic-client
  (export
   (get-example 0)
   (post-json-example 0)
   (post-form-example 0)
   (custom-headers-example 0)))

(defun get-example ()
  "Simple GET request example"
  ;; Start required applications
  (inets:start)
  (ssl:start)

  ;; Make a simple GET request
  (case (http.c:request "https://httpbin.org/get")
    (`#(ok ,response)
     (io:format "Status: ~p~n" `(,(mref response 'status)))
     (io:format "Body: ~p~n" `(,(mref response 'body)))
     response)
    (`#(error ,reason)
     (io:format "Error: ~p~n" `(,reason))
     reason)))

(defun post-json-example ()
  "POST request with JSON body"
  (inets:start)
  (ssl:start)

  ;; Create a request with JSON content
  (let* ((data #"{\"name\":\"LFE\",\"version\":\"2.1\",\"type\":\"language\"}")
         (req (http.request:new #"POST" "https://httpbin.org/post"))
         (req-with-json (http.request:set-json req data)))

    (case (http.c:request req-with-json)
      (`#(ok ,response)
       (io:format "Status: ~p~n" `(,(mref response 'status)))
       (io:format "Response body: ~p~n" `(,(mref response 'body)))
       response)
      (`#(error ,reason)
       (io:format "Error: ~p~n" `(,reason))
       reason))))

(defun post-form-example ()
  "POST request with form-encoded data"
  (inets:start)
  (ssl:start)

  ;; Create form data from a map
  (let* ((form-data #m(#"username" #"lfe-user"
                       #"password" #"secret123"
                       #"remember" #"true"))
         (req (http.request:new #"POST" "https://httpbin.org/post"))
         (req-with-form (http.request:set-form req form-data)))

    (case (http.c:request req-with-form)
      (`#(ok ,response)
       (io:format "Form submitted successfully!~n")
       (io:format "Status: ~p~n" `(,(mref response 'status)))
       response)
      (`#(error ,reason)
       (io:format "Error: ~p~n" `(,reason))
       reason))))

(defun custom-headers-example ()
  "Request with custom headers"
  (inets:start)
  (ssl:start)

  ;; Build request with custom headers using the fluent API
  (let* ((req (http.request:new #"GET" "https://httpbin.org/headers"))
         (req2 (http.request:set-header req #"User-Agent" #"LFE-HTTP/1.0.0"))
         (req3 (http.request:set-header req2 #"Accept" #"application/json"))
         (req4 (http.request:set-header req3 #"X-Custom-Header" #"my-value")))

    (case (http.c:request req4)
      (`#(ok ,response)
       (io:format "Request sent with custom headers~n")
       (io:format "Server echoed headers: ~p~n" `(,(mref response 'body)))
       response)
      (`#(error ,reason)
       (io:format "Error: ~p~n" `(,reason))
       reason))))

;;; Usage from LFE REPL:
;;;
;;; > (c "examples/basic-client.lfe")
;;; > (basic-client:get-example)
;;; > (basic-client:post-json-example)
;;; > (basic-client:post-form-example)
;;; > (basic-client:custom-headers-example)
