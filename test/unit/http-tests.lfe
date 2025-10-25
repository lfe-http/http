(defmodule http-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; Legacy API tests
;;; ---------------------------------------------------------------------------

(deftest default-headers
  (let ((headers (http:default-headers)))
    (is-equal 'true (is_map headers))
    (is-equal 'true (maps:is_key #"User-Agent" headers))
    (is-equal 'true (maps:is_key #"Accept" headers))))

(deftest default-version
  (is-equal 1.1 (http:default-version)))

(deftest methods
  (let ((methods (http:methods)))
    (is-equal 'true (is_list methods))
    (is-equal 'true (lists:member 'get methods))
    (is-equal 'true (lists:member 'post methods))))

(deftest versions
  (let ((versions (http:versions)))
    (is-equal 'true (is_list versions))
    (is-equal 'true (lists:member 1.1 versions))
    (is-equal 'true (lists:member 2 versions))))

;;; ---------------------------------------------------------------------------
;;; Binary method API tests
;;; ---------------------------------------------------------------------------

(deftest valid-method-get
  (is-equal 'true (http:valid-method? #"GET")))

(deftest valid-method-post
  (is-equal 'true (http:valid-method? #"POST")))

(deftest valid-method-put
  (is-equal 'true (http:valid-method? #"PUT")))

(deftest valid-method-delete
  (is-equal 'true (http:valid-method? #"DELETE")))

(deftest valid-method-patch
  (is-equal 'true (http:valid-method? #"PATCH")))

(deftest valid-method-head
  (is-equal 'true (http:valid-method? #"HEAD")))

(deftest valid-method-options
  (is-equal 'true (http:valid-method? #"OPTIONS")))

(deftest valid-method-invalid
  (is-equal 'false (http:valid-method? #"INVALID")))

(deftest valid-method-non-binary
  (is-equal 'false (http:valid-method? "GET"))
  (is-equal 'false (http:valid-method? 'get)))

(deftest normalize-method-binary
  (is-equal #"GET" (http:normalize-method #"get"))
  (is-equal #"POST" (http:normalize-method #"Post")))

(deftest normalize-method-atom
  (is-equal #"GET" (http:normalize-method 'get))
  (is-equal #"POST" (http:normalize-method 'POST)))

(deftest normalize-method-string
  (is-equal #"GET" (http:normalize-method "get"))
  (is-equal #"POST" (http:normalize-method "post")))

(deftest method-has-body-post
  (is-equal 'true (http:method-has-body? #"POST")))

(deftest method-has-body-put
  (is-equal 'true (http:method-has-body? #"PUT")))

(deftest method-has-body-patch
  (is-equal 'true (http:method-has-body? #"PATCH")))

(deftest method-has-body-get
  (is-equal 'false (http:method-has-body? #"GET")))

(deftest method-has-body-head
  (is-equal 'false (http:method-has-body? #"HEAD")))

(deftest method-has-body-delete
  (is-equal 'false (http:method-has-body? #"DELETE")))
