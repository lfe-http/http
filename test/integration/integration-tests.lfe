(defmodule integration-tests
  (behaviour ltest-integration))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; IMPORTANT: Integration Test Requirements
;;; ---------------------------------------------------------------------------
;;;
;;; These tests require:
;;; 1. Network access to https://httpbin.org
;;; 2. SSL/TLS support (ssl application must be started)
;;; 3. Valid SSL certificates and trust chain
;;;
;;; These tests may fail due to:
;;; - Network connectivity issues
;;; - Firewall restrictions
;;; - SSL/TLS configuration or certificate validation issues
;;; - httpbin.org service availability
;;;
;;; Test failures here do NOT indicate library bugs if all unit tests pass.
;;; These are primarily for manual verification of real-world HTTP requests.

;;; ---------------------------------------------------------------------------
;;; GET request tests
;;; ---------------------------------------------------------------------------

(deftest get-request-basic
  (progn
    (inets:start)
    (ssl:start)
    (case (http.c:request #"GET" "https://httpbin.org/get")
      (`#(ok ,resp)
       (progn
         (is-equal 200 (mref resp 'status))
         (is (is_binary (mref resp 'body)))
         (is (is_map (mref resp 'headers)))))
      (err
       (is-equal 'ok err)))))

(deftest get-request-with-query-params
  (progn
    (inets:start)
    (ssl:start)
    (let* ((req-a (http.request:new "https://httpbin.org/get"))
           (req-b (http.request:add-query-param req-a #"key1" #"value1"))
           (req (http.request:add-query-param req-b #"key2" #"value2")))
      (case (http.c:request req)
        (`#(ok ,resp)
         (let ((body (mref resp 'body)))
           (is-not-equal 'nomatch (binary:match body #"key1" '()))
           (is-not-equal 'nomatch (binary:match body #"value1" '()))))
        (err
         (is-equal 'ok err))))))

(deftest get-request-with-custom-headers
  (progn
    (inets:start)
    (ssl:start)
    (let* ((req-a (http.request:new "https://httpbin.org/headers"))
           (req (http.request:set-header req-a #"X-Custom-Header" #"test-value")))
      (case (http.c:request req)
        (`#(ok ,resp)
         (let ((body (mref resp 'body)))
           (is-not-equal 'nomatch (binary:match body #"X-Custom-Header" '()))))
        (err
         (is-equal 'ok err))))))

(deftest get-request-with-user-agent
  (progn
    (inets:start)
    (ssl:start)
    (let* ((req-a (http.request:new "https://httpbin.org/user-agent"))
           (req (http.request:set-header req-a #"User-Agent" #"lfe-http-test/1.0")))
      (case (http.c:request req)
        (`#(ok ,resp)
         (let ((body (mref resp 'body)))
           (is-not-equal 'nomatch (binary:match body #"lfe-http-test" '()))))
        (err
         (is-equal 'ok err))))))

;;; ---------------------------------------------------------------------------
;;; POST request tests
;;; ---------------------------------------------------------------------------

(deftest post-request-with-json
  (progn
    (inets:start)
    (ssl:start)
    (let* ((body #"{\"test\":\"data\"}")
           (req-a (http.request:new #"POST" "https://httpbin.org/post"))
           (req (http.request:set-json req-a body)))
      (case (http.c:request req)
        (`#(ok ,resp)
         (progn
           (is-equal 200 (mref resp 'status))
           (is-not-equal 'nomatch (binary:match (mref resp 'body) #"test" '()))))
        (err
         (is-equal 'ok err))))))

;;; ---------------------------------------------------------------------------
;;; PUT request tests
;;; ---------------------------------------------------------------------------

(deftest put-request-with-body
  (progn
    (inets:start)
    (ssl:start)
    (let* ((body #"test data")
           (req (http.request:new #"PUT" "https://httpbin.org/put" body)))
      (case (http.c:request req)
        (`#(ok ,resp)
         (is-equal 200 (mref resp 'status)))
        (err
         (is-equal 'ok err))))))

;;; ---------------------------------------------------------------------------
;;; PATCH request tests
;;; ---------------------------------------------------------------------------

(deftest patch-request
  (progn
    (inets:start)
    (ssl:start)
    (let* ((body #"{\"patch\":\"data\"}")
           (req-a (http.request:new #"PATCH" "https://httpbin.org/patch"))
           (req (http.request:set-json req-a body)))
      (case (http.c:request req)
        (`#(ok ,resp)
         (is-equal 200 (mref resp 'status)))
        (err
         (is-equal 'ok err))))))

;;; ---------------------------------------------------------------------------
;;; DELETE request tests
;;; ---------------------------------------------------------------------------

(deftest delete-request
  (progn
    (inets:start)
    (ssl:start)
    (case (http.c:request #"DELETE" "https://httpbin.org/delete")
      (`#(ok ,resp)
       (is-equal 200 (mref resp 'status)))
      (err
       (is-equal 'ok err)))))

;;; ---------------------------------------------------------------------------
;;; Status code tests
;;; ---------------------------------------------------------------------------

(deftest handle-404-status
  (progn
    (inets:start)
    (ssl:start)
    (case (http.c:request #"GET" "https://httpbin.org/status/404")
      (`#(ok ,resp)
       (is-equal 404 (mref resp 'status)))
      (err
       (is-equal 'ok err)))))

(deftest handle-500-status
  (progn
    (inets:start)
    (ssl:start)
    (case (http.c:request #"GET" "https://httpbin.org/status/500")
      (`#(ok ,resp)
       (is-equal 500 (mref resp 'status)))
      (err
       (is-equal 'ok err)))))

;;; ---------------------------------------------------------------------------
;;; Redirect tests
;;; ---------------------------------------------------------------------------

(deftest handle-redirect
  (progn
    (inets:start)
    (ssl:start)
    (case (http.c:request #"GET" "https://httpbin.org/redirect/1")
      (`#(ok ,resp)
       (is-equal 200 (mref resp 'status)))
      (err
       (is-equal 'ok err)))))
