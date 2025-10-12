(defmodule http.util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; Binary conversion tests
;;; ---------------------------------------------------------------------------

(deftest ensure-binary-with-binary
  (is-equal #"test" (http.util:ensure-binary #"test")))

(deftest ensure-binary-with-list
  (is-equal #"test" (http.util:ensure-binary "test")))

(deftest ensure-binary-with-atom
  (is-equal #"test" (http.util:ensure-binary 'test)))

(deftest ensure-binary-with-integer
  (is-equal #"123" (http.util:ensure-binary 123)))

(deftest ensure-binary-with-iolist
  (is-equal #"hello world"
            (http.util:ensure-binary '(#"hello" " " "world"))))

(deftest binary-upcase-lowercase
  (is-equal #"GET" (http.util:binary-upcase #"get")))

(deftest binary-upcase-mixed
  (is-equal #"POST" (http.util:binary-upcase #"Post")))

(deftest binary-upcase-already-upper
  (is-equal #"ALREADY" (http.util:binary-upcase #"ALREADY")))

(deftest binary-downcase-uppercase
  (is-equal #"get" (http.util:binary-downcase #"GET")))

(deftest binary-downcase-mixed
  (is-equal #"content-type" (http.util:binary-downcase #"Content-Type")))

(deftest binary-downcase-atom-test
  (is-equal 'get (http.util:binary-downcase-atom #"GET")))

;;; ---------------------------------------------------------------------------
;;; HTTP version utilities tests
;;; ---------------------------------------------------------------------------

(deftest http-version-string-1.0
  (is-equal #"HTTP/1.0" (http.util:http-version-string 1.0)))

(deftest http-version-string-1.1
  (is-equal #"HTTP/1.1" (http.util:http-version-string 1.1)))

(deftest http-version-string-2
  (is-equal #"HTTP/2" (http.util:http-version-string 2)))

(deftest http-version-string-3
  (is-equal #"HTTP/3" (http.util:http-version-string 3)))

(deftest http-version-tuple-1.0
  (is-equal #(1 0) (http.util:http-version-tuple 1.0)))

(deftest http-version-tuple-1.1
  (is-equal #(1 1) (http.util:http-version-tuple 1.1)))

(deftest http-version-tuple-2
  (is-equal #(2 0) (http.util:http-version-tuple 2)))

;;; ---------------------------------------------------------------------------
;;; URL utilities tests
;;; ---------------------------------------------------------------------------

(deftest join-path-simple
  (is-equal #"/api/v1/users"
            (http.util:join-path '(#"api" #"v1" #"users"))))

(deftest join-path-empty
  (is-equal #"/"
            (http.util:join-path '())))

(deftest query-string-empty
  (is-equal #"" (http.util:query-string #m())))

(deftest query-string-single-param
  (is-equal #"key=value"
            (http.util:query-string #m(#"key" #"value"))))

(deftest query-string-multiple-params
  (let ((qs (http.util:query-string #m(#"a" #"1" #"b" #"2"))))
    ;; Order is not guaranteed in maps, so check both possibilities
    (is-equal 'true (orelse (== qs #"a=1&b=2")
                            (== qs #"b=2&a=1")))))

;;; ---------------------------------------------------------------------------
;;; Performance utilities tests
;;; ---------------------------------------------------------------------------

(deftest measure-basic
  (let ((`#(,result ,elapsed)
         (http.util:measure
           (lambda ()
             (timer:sleep 10)
             'ok))))
    (is-equal 'ok result)
    ;; Should take at least 10ms (10000 microseconds)
    (is-equal 'true (>= elapsed 10000))))

(deftest measure-with-unit
  (let ((`#(,result ,elapsed)
         (http.util:measure
           (lambda ()
             (timer:sleep 10)
             'ok)
           'millisecond)))
    (is-equal 'ok result)
    ;; Should take at least 10 milliseconds
    (is-equal 'true (>= elapsed 10))))

;;; ---------------------------------------------------------------------------
;;; Header utilities tests
;;; ---------------------------------------------------------------------------

(deftest add-header-basic
  (let* ((req #m(headers #m() version 1.1))
         (updated (http.util:add-header req #"Content-Type" #"text/plain")))
    (is-equal #"text/plain"
              (maps:get #"Content-Type" (mref updated 'headers)))))

(deftest add-header-converts-types
  (let* ((req #m(headers #m() version 1.1))
         (updated (http.util:add-header req "Content-Type" 'text/plain)))
    (is-equal #"text/plain"
              (maps:get #"Content-Type" (mref updated 'headers)))))
