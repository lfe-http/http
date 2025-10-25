;;;; Response Helper Examples
;;;; Demonstrates convenient response building functions

(defmodule response-helpers
  (export
   (success-responses 0)
   (error-responses 0)
   (content-responses 0)
   (custom-response 0)))

(defun success-responses ()
  "Demonstrate success response helpers"
  (io:format "~n=== Success Responses ===~n")

  ;; 200 OK
  (let ((resp1 (http.response:ok #"Request successful")))
    (io:format "OK: ~p~n" `(,(mref resp1 'status))))

  ;; 200 OK with headers
  (let* ((headers #m(#"X-Custom" #"value"))
         (resp2 (http.response:ok #"Success with headers" headers)))
    (io:format "OK with headers: ~p~n" `(,(mref resp2 'headers))))

  ;; 201 Created
  (let ((resp3 (http.response:created #"Resource created")))
    (io:format "Created: ~p~n" `(,(mref resp3 'status))))

  ;; 202 Accepted
  (let ((resp4 (http.response:accepted #"Request accepted for processing")))
    (io:format "Accepted: ~p~n" `(,(mref resp4 'status))))

  ;; 204 No Content
  (let ((resp5 (http.response:no-content)))
    (io:format "No Content: ~p (body: ~p)~n"
               `(,(mref resp5 'status) ,(mref resp5 'body))))

  'ok)

(defun error-responses ()
  "Demonstrate error response helpers"
  (io:format "~n=== Error Responses ===~n")

  ;; 400 Bad Request
  (let ((resp1 (http.response:bad-request #"Invalid input")))
    (io:format "Bad Request: ~p~n" `(,(mref resp1 'status))))

  ;; 401 Unauthorized
  (let ((resp2 (http.response:unauthorized #"Authentication required")))
    (io:format "Unauthorized: ~p~n" `(,(mref resp2 'status))))

  ;; 403 Forbidden
  (let ((resp3 (http.response:forbidden #"Access denied")))
    (io:format "Forbidden: ~p~n" `(,(mref resp3 'status))))

  ;; 404 Not Found
  (let ((resp4 (http.response:not-found #"Resource not found")))
    (io:format "Not Found: ~p~n" `(,(mref resp4 'status))))

  ;; 500 Internal Server Error
  (let ((resp5 (http.response:error #"Something went wrong")))
    (io:format "Error: ~p~n" `(,(mref resp5 'status))))

  ;; 502 Bad Gateway
  (let ((resp6 (http.response:bad-gateway #"Upstream service error")))
    (io:format "Bad Gateway: ~p~n" `(,(mref resp6 'status))))

  ;; 503 Service Unavailable
  (let ((resp7 (http.response:unavailable #"Service temporarily down")))
    (io:format "Unavailable: ~p~n" `(,(mref resp7 'status))))

  'ok)

(defun content-responses ()
  "Demonstrate content-type specific response helpers"
  (io:format "~n=== Content-Type Responses ===~n")

  ;; JSON response
  (let* ((data #"{\"message\":\"Hello, LFE!\",\"status\":\"success\"}")
         (resp1 (http.response:json 200 data)))
    (io:format "JSON: ~p~n" `(,(http.header:get (mref resp1 'headers)
                                                 #"Content-Type"
                                                 'undefined
                                                 #m(case-insensitive true)))))

  ;; HTML response
  (let* ((html #"<html><body><h1>Hello!</h1></body></html>")
         (resp2 (http.response:html 200 html)))
    (io:format "HTML: ~p~n" `(,(http.header:get (mref resp2 'headers)
                                                 #"Content-Type"
                                                 'undefined
                                                 #m(case-insensitive true)))))

  ;; Plain text response
  (let* ((text #"Hello, World!")
         (resp3 (http.response:text 200 text)))
    (io:format "Text: ~p~n" `(,(http.header:get (mref resp3 'headers)
                                                 #"Content-Type"
                                                 'undefined
                                                 #m(case-insensitive true)))))

  ;; XML response
  (let* ((xml #"<?xml version=\"1.0\"?><root><msg>Hello</msg></root>")
         (resp4 (http.response:xml 200 xml)))
    (io:format "XML: ~p~n" `(,(http.header:get (mref resp4 'headers)
                                                #"Content-Type"
                                                'undefined
                                                #m(case-insensitive true)))))

  'ok)

(defun custom-response ()
  "Build a custom response from scratch"
  (io:format "~n=== Custom Response ===~n")

  ;; Start with empty response
  (let* ((resp1 (http.response:new))
         ;; Set status
         (resp2 (http.response:set-status resp1 201))
         ;; Add headers
         (resp3 (http.response:set-header resp2 #"Location" #"/api/users/123"))
         (resp4 (http.response:set-header resp3 #"X-Request-ID" #"abc-def-789"))
         ;; Set body
         (body #"{\"id\":123,\"created\":true}")
         (resp5 (http.response:set-body resp4 body))
         ;; Add Content-Type
         (resp6 (http.response:set-header resp5 #"Content-Type" #"application/json")))

    (io:format "Status: ~p~n" `(,(mref resp6 'status)))
    (io:format "Headers: ~p~n" `(,(mref resp6 'headers)))
    (io:format "Body: ~p~n" `(,(mref resp6 'body)))

    resp6))

;;; Usage from LFE REPL:
;;;
;;; > (c "examples/response-helpers.lfe")
;;; > (response-helpers:success-responses)
;;; > (response-helpers:error-responses)
;;; > (response-helpers:content-responses)
;;; > (response-helpers:custom-response)
