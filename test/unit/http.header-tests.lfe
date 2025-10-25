(defmodule http.header-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; ---------------------------------------------------------------------------
;;; Legacy API tests (ensure backward compatibility)
;;; ---------------------------------------------------------------------------

(deftest add-with-binary-keys
  (let ((hdrs (http.header:new)))
    (let ((hdrs (http.header:add hdrs #"content-type" #"text/plain")))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))))

(deftest add-with-string-keys
  (let ((hdrs (http.header:new)))
    (let ((hdrs (http.header:add hdrs "content-type" "text/plain")))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))))

(deftest add-with-atom-keys
  (let ((hdrs (http.header:new)))
    (let ((hdrs (http.header:add hdrs 'content-type 'text/plain)))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))))

(deftest add-with-tuple
  (let ((hdrs (http.header:new)))
    (let ((hdrs (http.header:add hdrs `#(content-type text/plain))))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs)))))

(deftest list->map-test
  (let ((hdrs (http.header:list->map
               '(#(content-type #"text/plain")
                 #("accept" application/json)
                 #(#"content-length" 5)
                 #(#"X-API-Key" "1234deadbeefcafe5678")))))
    (is-equal '(#"X-API-Key" #"accept" #"content-length" #"content-type")
              (lists:sort (maps:keys hdrs)))
    (is-equal '(#"1234deadbeefcafe5678"
                #"5"
                #"application/json"
                #"text/plain")
              (lists:sort (maps:values hdrs)))))

;;; ---------------------------------------------------------------------------
;;; New API tests
;;; ---------------------------------------------------------------------------

(deftest from-list-single-pass
  (let ((hdrs (http.header:from-list
               '(#(content-type #"text/html")
                 #(accept #"application/json")))))
    (is-equal #"text/html" (maps:get #"content-type" hdrs))
    (is-equal #"application/json" (maps:get #"accept" hdrs))))

(deftest from-list-mixed-types
  (let ((hdrs (http.header:from-list
               '(#("content-type" "text/html")
                 #(accept application/json)
                 #(#"content-length" 42)))))
    (is-equal #"text/html" (maps:get #"content-type" hdrs))
    (is-equal #"application/json" (maps:get #"accept" hdrs))
    (is-equal #"42" (maps:get #"content-length" hdrs))))

(deftest to-list-sorted
  (let* ((hdrs (http.header:from-list
                '(#(z-header #"value3")
                  #(a-header #"value1")
                  #(m-header #"value2"))))
         (list-form (http.header:to-list hdrs)))
    (is-equal '(#(#"a-header" #"value1")
                #(#"m-header" #"value2")
                #(#"z-header" #"value3"))
              list-form)))

;;; ---------------------------------------------------------------------------
;;; Case-sensitive lookup tests
;;; ---------------------------------------------------------------------------

(deftest get-case-sensitive
  (let ((hdrs (http.header:from-list
               '(#(Content-Type #"text/html")))))
    (is-equal #"text/html" (http.header:get hdrs #"Content-Type"))
    (is-equal 'undefined (http.header:get hdrs #"content-type"))))

(deftest get-with-default
  (let ((hdrs (http.header:new)))
    (is-equal 'not-found (http.header:get hdrs #"Missing" 'not-found))))

(deftest has-key-case-sensitive
  (let ((hdrs (http.header:from-list
               '(#(Content-Type #"text/html")))))
    (is-equal 'true (http.header:has-key? hdrs #"Content-Type"))
    (is-equal 'false (http.header:has-key? hdrs #"content-type"))))

;;; ---------------------------------------------------------------------------
;;; Case-insensitive lookup tests
;;; ---------------------------------------------------------------------------

(deftest get-ci-lowercase
  (let ((hdrs (http.header:from-list
               '(#(Content-Type #"text/html")))))
    (is-equal #"text/html" (http.header:get hdrs #"content-type" #m(case-insensitive true)))))

(deftest get-ci-uppercase
  (let ((hdrs (http.header:from-list
               '(#(content-type #"text/html")))))
    (is-equal #"text/html" (http.header:get hdrs #"CONTENT-TYPE" #m(case-insensitive true)))))

(deftest get-ci-mixed-case
  (let ((hdrs (http.header:from-list
               '(#(Content-Type #"text/html")))))
    (is-equal #"text/html" (http.header:get hdrs #"CoNtEnT-tYpE" #m(case-insensitive true)))))

(deftest get-ci-with-default
  (let ((hdrs (http.header:new)))
    (is-equal 'not-found (http.header:get hdrs #"missing" 'not-found #m(case-insensitive true)))))

(deftest has-key-ci-lowercase
  (let ((hdrs (http.header:from-list
               '(#(Content-Type #"text/html")))))
    (is-equal 'true (http.header:has-key? hdrs #"content-type" #m(case-insensitive true)))))

(deftest has-key-ci-uppercase
  (let ((hdrs (http.header:from-list
               '(#(content-type #"text/html")))))
    (is-equal 'true (http.header:has-key? hdrs #"CONTENT-TYPE" #m(case-insensitive true)))))

(deftest has-key-ci-not-found
  (let ((hdrs (http.header:from-list
               '(#(content-type #"text/html")))))
    (is-equal 'false (http.header:has-key? hdrs #"missing" #m(case-insensitive true)))))

;;; ---------------------------------------------------------------------------
;;; Bulk operations tests
;;; ---------------------------------------------------------------------------

(deftest merge-headers
  (let* ((hdrs1 (http.header:from-list
                 '(#(content-type #"text/html")
                   #(accept #"*/*"))))
         (hdrs2 (http.header:from-list
                 '(#(content-type #"application/json")
                   #(x-custom #"value"))))
         (merged (http.header:merge hdrs1 hdrs2)))
    ;; hdrs2 takes precedence
    (is-equal #"application/json" (maps:get #"content-type" merged))
    (is-equal #"*/*" (maps:get #"accept" merged))
    (is-equal #"value" (maps:get #"x-custom" merged))))

(deftest filter-headers
  (let* ((hdrs (http.header:from-list
                '(#(content-type #"text/html")
                  #(x-custom-1 #"value1")
                  #(accept #"*/*")
                  #(x-custom-2 #"value2"))))
         (filtered (http.header:filter
                    (lambda (kv)
                      (let ((`#(,k ,_) kv))
                        ;; Keep only headers starting with "x-"
                        (== (binary:longest_common_prefix (list k #"x-")) 2)))
                    hdrs)))
    (is-equal 2 (maps:size filtered))
    (is-equal 'true (maps:is_key #"x-custom-1" filtered))
    (is-equal 'true (maps:is_key #"x-custom-2" filtered))
    (is-equal 'false (maps:is_key #"content-type" filtered))))

(deftest remove-header
  (let* ((hdrs (http.header:from-list
                '(#(content-type #"text/html")
                  #(accept #"*/*")
                  #(x-custom #"value"))))
         (removed (http.header:remove hdrs #"accept")))
    (is-equal 2 (maps:size removed))
    (is-equal 'false (maps:is_key #"accept" removed))
    (is-equal 'true (maps:is_key #"content-type" removed))))

;;; ---------------------------------------------------------------------------
;;; Utility operations tests
;;; ---------------------------------------------------------------------------

(deftest keys-test
  (let ((hdrs (http.header:from-list
               '(#(a #"1") #(b #"2") #(c #"3")))))
    (is-equal '(#"a" #"b" #"c")
              (lists:sort (http.header:keys hdrs)))))

(deftest values-test
  (let ((hdrs (http.header:from-list
               '(#(a #"1") #(b #"2") #(c #"3")))))
    (is-equal '(#"1" #"2" #"3")
              (lists:sort (http.header:values hdrs)))))

(deftest normalize-key-test
  (is-equal #"content-type" (http.header:normalize-key #"content-type"))
  (is-equal #"content-type" (http.header:normalize-key "content-type"))
  (is-equal #"content-type" (http.header:normalize-key 'content-type)))

;;; ---------------------------------------------------------------------------
;;; Performance - single pass vs multi-pass
;;; ---------------------------------------------------------------------------

(deftest large-list-conversion
  (let* ((large-list (lists:map
                      (lambda (n)
                        `#(,(list_to_binary (io_lib:format "header-~p" (list n)))
                           ,(list_to_binary (io_lib:format "value-~p" (list n)))))
                      (lists:seq 1 100)))
         (hdrs (http.header:from-list large-list)))
    (is-equal 100 (maps:size hdrs))
    (is-equal #"value-50" (maps:get #"header-50" hdrs))))
