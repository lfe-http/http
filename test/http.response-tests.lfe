(defmodule http.response-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; Constructor Tests
;;; ---------------------------------------------------------------------------

(deftest new-empty
  (let ((r (http.response:new)))
    (is-equal 200 (mref r 'status))
    (is-equal #m() (mref r 'headers))
    (is-equal #"" (mref r 'body))
    (is-equal 1.1 (mref r 'version))))

(deftest new-with-status
  (let ((r (http.response:new 404)))
    (is-equal 404 (mref r 'status))))

(deftest new-with-status-and-body
  (let ((r (http.response:new 200 #"OK")))
    (is-equal 200 (mref r 'status))
    (is-equal #"OK" (mref r 'body))))

(deftest new-with-all-params
  (let ((r (http.response:new 201 #m(#"X-Custom" #"value") #"Created")))
    (is-equal 201 (mref r 'status))
    (is-equal #"value" (maps:get #"X-Custom" (mref r 'headers)))
    (is-equal #"Created" (mref r 'body))))

;;; ---------------------------------------------------------------------------
;;; Setter Tests
;;; ---------------------------------------------------------------------------

(deftest set-status-test
  (let* ((r (http.response:new))
         (r2 (http.response:set-status r 404)))
    (is-equal 404 (mref r2 'status))))

(deftest set-body-binary
  (let* ((r (http.response:new))
         (r2 (http.response:set-body r #"test")))
    (is-equal #"test" (mref r2 'body))))

(deftest set-body-string
  (let* ((r (http.response:new))
         (r2 (http.response:set-body r "test")))
    (is-equal #"test" (mref r2 'body))))

(deftest set-body-integer
  (let* ((r (http.response:new))
         (r2 (http.response:set-body r 123)))
    (is-equal #"123" (mref r2 'body))))

(deftest set-body-atom
  (let* ((r (http.response:new))
         (r2 (http.response:set-body r 'ok)))
    (is-equal #"ok" (mref r2 'body))))

(deftest set-header-test
  (let* ((r (http.response:new))
         (r2 (http.response:set-header r #"X-Custom" #"value")))
    (is-equal #"value" (maps:get #"X-Custom" (mref r2 'headers)))))

(deftest add-header-test
  (let* ((r (http.response:new))
         (r2 (http.response:add-header r #"X-Custom" #"value")))
    (is-equal #"value" (maps:get #"X-Custom" (mref r2 'headers)))))

(deftest remove-header-test
  (let* ((r (http.response:new))
         (r2 (http.response:set-header r #"X-Custom" #"value"))
         (r3 (http.response:remove-header r2 #"X-Custom")))
    (is-equal 'false (maps:is_key #"X-Custom" (mref r3 'headers)))))

;;; ---------------------------------------------------------------------------
;;; Getter Tests
;;; ---------------------------------------------------------------------------

(deftest getters-test
  (let ((r (http.response:new 201 #m(#"X-Test" #"val") #"body")))
    (is-equal 201 (http.response:status r))
    (is-equal #"body" (http.response:body r))
    (is-equal #m(#"X-Test" #"val") (http.response:headers r))))

;;; ---------------------------------------------------------------------------
;;; Convenience Builder Tests (2xx)
;;; ---------------------------------------------------------------------------

(deftest ok-no-body
  (is-equal 200 (mref (http.response:ok) 'status)))

(deftest ok-with-body
  (let ((r (http.response:ok #"OK")))
    (is-equal 200 (mref r 'status))
    (is-equal #"OK" (mref r 'body))))

(deftest created-no-body
  (is-equal 201 (mref (http.response:created) 'status)))

(deftest created-with-body
  (let ((r (http.response:created #"Created")))
    (is-equal 201 (mref r 'status))
    (is-equal #"Created" (mref r 'body))))

(deftest accepted-no-body
  (is-equal 202 (mref (http.response:accepted) 'status)))

(deftest accepted-with-body
  (let ((r (http.response:accepted #"Accepted")))
    (is-equal 202 (mref r 'status))
    (is-equal #"Accepted" (mref r 'body))))

(deftest no-content-test
  (is-equal 204 (mref (http.response:no-content) 'status)))

;;; ---------------------------------------------------------------------------
;;; Convenience Builder Tests (4xx)
;;; ---------------------------------------------------------------------------

(deftest bad-request-no-body
  (is-equal 400 (mref (http.response:bad-request) 'status)))

(deftest bad-request-with-body
  (let ((r (http.response:bad-request #"Bad")))
    (is-equal 400 (mref r 'status))
    (is-equal #"Bad" (mref r 'body))))

(deftest unauthorized-no-body
  (is-equal 401 (mref (http.response:unauthorized) 'status)))

(deftest unauthorized-with-body
  (let ((r (http.response:unauthorized #"Unauth")))
    (is-equal 401 (mref r 'status))
    (is-equal #"Unauth" (mref r 'body))))

(deftest forbidden-no-body
  (is-equal 403 (mref (http.response:forbidden) 'status)))

(deftest forbidden-with-body
  (let ((r (http.response:forbidden #"Forbid")))
    (is-equal 403 (mref r 'status))
    (is-equal #"Forbid" (mref r 'body))))

(deftest not-found-no-body
  (is-equal 404 (mref (http.response:not-found) 'status)))

(deftest not-found-with-body
  (let ((r (http.response:not-found #"Not Found")))
    (is-equal 404 (mref r 'status))
    (is-equal #"Not Found" (mref r 'body))))

;;; ---------------------------------------------------------------------------
;;; Convenience Builder Tests (5xx)
;;; ---------------------------------------------------------------------------

(deftest error-no-body
  (is-equal 500 (mref (http.response:error) 'status)))

(deftest error-with-body
  (let ((r (http.response:error #"Error")))
    (is-equal 500 (mref r 'status))
    (is-equal #"Error" (mref r 'body))))

(deftest bad-gateway-no-body
  (is-equal 502 (mref (http.response:bad-gateway) 'status)))

(deftest bad-gateway-with-body
  (let ((r (http.response:bad-gateway #"Gateway")))
    (is-equal 502 (mref r 'status))
    (is-equal #"Gateway" (mref r 'body))))

(deftest unavailable-no-body
  (is-equal 503 (mref (http.response:unavailable) 'status)))

(deftest unavailable-with-body
  (let ((r (http.response:unavailable #"Unavail")))
    (is-equal 503 (mref r 'status))
    (is-equal #"Unavail" (mref r 'body))))

;;; ---------------------------------------------------------------------------
;;; Content-Type Helper Tests
;;; ---------------------------------------------------------------------------

(deftest json-helper
  (let ((r (http.response:json 200 #"{\"key\":\"value\"}")))
    (is-equal 200 (mref r 'status))
    (is-equal #"{\"key\":\"value\"}" (mref r 'body))
    (is-equal #"application/json; charset=utf-8"
              (http.header:get (mref r 'headers) #"Content-Type"))))

(deftest text-helper
  (let ((r (http.response:text 200 #"Plain text")))
    (is-equal 200 (mref r 'status))
    (is-equal #"Plain text" (mref r 'body))
    (is-equal #"text/plain; charset=utf-8"
              (http.header:get (mref r 'headers) #"Content-Type"))))

(deftest html-helper
  (let ((r (http.response:html 200 #"<html></html>")))
    (is-equal 200 (mref r 'status))
    (is-equal #"<html></html>" (mref r 'body))
    (is-equal #"text/html; charset=utf-8"
              (http.header:get (mref r 'headers) #"Content-Type"))))

(deftest xml-helper
  (let ((r (http.response:xml 200 #"<xml></xml>")))
    (is-equal 200 (mref r 'status))
    (is-equal #"<xml></xml>" (mref r 'body))
    (is-equal #"application/xml; charset=utf-8"
              (http.header:get (mref r 'headers) #"Content-Type"))))
