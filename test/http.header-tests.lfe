(defmodule http-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest add
  (let ((hdrs (http.header:make)))
    (let ((hdrs (http.header:add hdrs #"content-type" #"text/plain")))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))
    (let ((hdrs (http.header:add hdrs "content-type" "text/plain")))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))
    (let ((hdrs (http.header:add hdrs 'content-type 'text/plain)))
      (is-equal #"text/plain" (maps:get #"content-type" hdrs))
      (is-equal '(#"content-type") (maps:keys hdrs)))))

(deftest list->map
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
