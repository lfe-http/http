(defmodule property-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; Note: These are simplified property tests
;;; For full property-based testing, consider using proper or triq

;;; ---------------------------------------------------------------------------
;;; Binary conversion property tests
;;; ---------------------------------------------------------------------------

(deftest ensure-binary-idempotency-binary
  (let* ((input #"test")
         (result1 (http.util:ensure-binary input))
         (result2 (http.util:ensure-binary result1)))
    (is-equal result1 result2)))

(deftest ensure-binary-idempotency-string
  (let* ((input "test")
         (result1 (http.util:ensure-binary input))
         (result2 (http.util:ensure-binary result1)))
    (is-equal result1 result2)))

(deftest ensure-binary-idempotency-atom
  (let* ((input 'test)
         (result1 (http.util:ensure-binary input))
         (result2 (http.util:ensure-binary result1)))
    (is-equal result1 result2)))

(deftest ensure-binary-idempotency-integer
  (let* ((input 123)
         (result1 (http.util:ensure-binary input))
         (result2 (http.util:ensure-binary result1)))
    (is-equal result1 result2)))

;;; ---------------------------------------------------------------------------
;;; Case conversion roundtrip tests
;;; ---------------------------------------------------------------------------

(deftest upcase-downcase-roundtrip-1
  (let* ((input #"content-type")
         (upper (http.util:binary-upcase input))
         (lower (http.util:binary-downcase input))
         (roundtrip (http.util:binary-downcase (http.util:binary-upcase lower))))
    (is-equal lower roundtrip)))

(deftest upcase-downcase-roundtrip-2
  (let* ((input #"ACCEPT")
         (upper (http.util:binary-upcase input))
         (lower (http.util:binary-downcase input))
         (roundtrip (http.util:binary-downcase (http.util:binary-upcase lower))))
    (is-equal lower roundtrip)))

(deftest upcase-downcase-roundtrip-3
  (let* ((input #"User-Agent")
         (upper (http.util:binary-upcase input))
         (lower (http.util:binary-downcase input))
         (roundtrip (http.util:binary-downcase (http.util:binary-upcase lower))))
    (is-equal lower roundtrip)))

;;; ---------------------------------------------------------------------------
;;; Header conversion reversibility tests
;;; ---------------------------------------------------------------------------

(deftest header-list-map-list-reversibility
  (let ((proplist '(#(#"Content-Type" #"text/html")
                    #(#"Accept" #"*/*")
                    #(#"User-Agent" #"test"))))
    (let* ((map (http.header:from-list proplist))
           (list2 (http.header:to-list map)))
      ;; Lists should be equal when sorted
      (is-equal (lists:sort proplist) (lists:sort list2)))))

;;; ---------------------------------------------------------------------------
;;; Request construction consistency tests
;;; ---------------------------------------------------------------------------

(deftest request-construction-consistency
  (let* ((method #"POST")
         (url "http://example.com")
         (body #"test")
         (headers #m(#"X-Custom" #"value"))
         ;; Different construction methods
         (req1 (http.request:new method url body headers))
         (req2-a (http.request:new url))
         (req2-b (http.request:set-method req2-a method))
         (req2-c (http.request:set-body req2-b body))
         (req2 (http.request:set-headers req2-c headers)))
    ;; Should produce equivalent results
    (is-equal (mref req1 'method) (mref req2 'method))
    (is-equal (mref req1 'body) (mref req2 'body))
    (is-equal (mref req1 'url) (mref req2 'url))))
