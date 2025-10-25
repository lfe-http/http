;;;; Header Management Examples
;;;; Demonstrates the http.header module capabilities

(defmodule headers
  (export
   (basic-operations 0)
   (case-insensitive 0)
   (bulk-operations 0)
   (header-utilities 0)))

(defun basic-operations ()
  "Demonstrate basic header operations"
  (io:format "~n=== Basic Header Operations ===~n")

  ;; Create empty headers
  (let ((h1 (http.header:new)))
    (io:format "Empty headers: ~p~n" `(,h1)))

  ;; Add single header
  (let* ((h1 (http.header:new))
         (h2 (http.header:add h1 #"Content-Type" #"application/json")))
    (io:format "After adding one: ~p~n" `(,h2)))

  ;; Add multiple headers
  (let* ((h1 (http.header:new))
         (h2 (http.header:add h1 #"Content-Type" #"application/json"))
         (h3 (http.header:add h2 #"Accept" #"application/json"))
         (h4 (http.header:add h3 #"User-Agent" #"LFE-HTTP/1.0.0")))
    (io:format "Multiple headers: ~p~n" `(,h4)))

  ;; Convert from list
  (let ((headers (http.header:from-list
                  '(#(#"Content-Type" #"text/html")
                    #(#"Content-Length" #"1234")
                    #(#"Server" #"LFE")))))
    (io:format "From list: ~p~n" `(,headers)))

  ;; Convert to list
  (let* ((h (http.header:from-list '(#(#"Accept" #"*/*"))))
         (lst (http.header:to-list h)))
    (io:format "To list: ~p~n" `(,lst)))

  'ok)

(defun case-insensitive ()
  "Demonstrate case-insensitive header operations"
  (io:format "~n=== Case-Insensitive Operations ===~n")

  (let ((headers (http.header:from-list
                  '(#(#"Content-Type" #"application/json")
                    #(#"Accept" #"text/html")
                    #(#"User-Agent" #"LFE")))))

    ;; Case-sensitive get (won't find it)
    (io:format "Case-sensitive get 'content-type': ~p~n"
               `(,(http.header:get headers #"content-type")))

    ;; Case-insensitive get (will find it)
    (io:format "Case-insensitive get 'content-type': ~p~n"
               `(,(http.header:get headers #"content-type" 'undefined
                                   #m(case-insensitive true))))

    ;; Works with any case variation
    (io:format "Get 'CONTENT-TYPE': ~p~n"
               `(,(http.header:get headers #"CONTENT-TYPE" 'undefined
                                   #m(case-insensitive true))))

    (io:format "Get 'Content-Type': ~p~n"
               `(,(http.header:get headers #"Content-Type" 'undefined
                                   #m(case-insensitive true))))

    ;; Check key existence (case-insensitive)
    (io:format "Has 'accept' key? ~p~n"
               `(,(http.header:has-key? headers #"accept"
                                        #m(case-insensitive true))))

    (io:format "Has 'ACCEPT' key? ~p~n"
               `(,(http.header:has-key? headers #"ACCEPT"
                                        #m(case-insensitive true))))

    (io:format "Has 'x-custom' key? ~p~n"
               `(,(http.header:has-key? headers #"x-custom"
                                        #m(case-insensitive true)))))

  'ok)

(defun bulk-operations ()
  "Demonstrate bulk header operations"
  (io:format "~n=== Bulk Operations ===~n")

  ;; Merge two header maps
  (let* ((h1 (http.header:from-list '(#(#"Content-Type" #"text/html"))))
         (h2 (http.header:from-list '(#(#"Accept" #"application/json")
                                      #(#"User-Agent" #"LFE"))))
         (merged (http.header:merge h1 h2)))
    (io:format "Merged headers: ~p~n" `(,merged)))

  ;; Filter headers
  (let* ((headers (http.header:from-list
                   '(#(#"Content-Type" #"text/html")
                     #(#"X-Custom-1" #"value1")
                     #(#"Accept" #"*/*")
                     #(#"X-Custom-2" #"value2"))))
         ;; Keep only headers that start with "X-"
         (filtered (http.header:filter
                    (lambda (k v)
                      (let ((key-str (binary_to_list k)))
                        (== (lists:prefix "x-" (string:lowercase key-str)) 'true)))
                    headers)))
    (io:format "Filtered (X- headers only): ~p~n" `(,filtered)))

  ;; Remove header
  (let* ((headers (http.header:from-list
                   '(#(#"Content-Type" #"text/html")
                     #(#"X-To-Remove" #"value")
                     #(#"Accept" #"*/*"))))
         (removed (http.header:remove headers #"X-To-Remove")))
    (io:format "After removal: ~p~n" `(,removed)))

  'ok)

(defun header-utilities ()
  "Demonstrate header utility functions"
  (io:format "~n=== Header Utilities ===~n")

  (let ((headers (http.header:from-list
                  '(#(#"Content-Type" #"application/json")
                    #(#"Accept" #"text/html")
                    #(#"User-Agent" #"LFE-HTTP/1.0.0")
                    #(#"Authorization" #"Bearer token123")))))

    ;; Get all keys
    (io:format "All keys: ~p~n" `(,(http.header:keys headers)))

    ;; Get all values
    (io:format "All values: ~p~n" `(,(http.header:values headers)))

    ;; Normalize key (to lowercase binary)
    (io:format "Normalized 'Content-Type': ~p~n"
               `(,(http.header:normalize-key #"Content-Type")))

    (io:format "Normalized 'ACCEPT': ~p~n"
               `(,(http.header:normalize-key #"ACCEPT")))

    ;; Works with strings and atoms too
    (io:format "Normalized \"User-Agent\": ~p~n"
               `(,(http.header:normalize-key "User-Agent")))

    (io:format "Normalized 'authorization: ~p~n"
               `(,(http.header:normalize-key 'authorization))))

  'ok)

;;; Usage from LFE REPL:
;;;
;;; > (c "examples/headers.lfe")
;;; > (headers:basic-operations)
;;; > (headers:case-insensitive)
;;; > (headers:bulk-operations)
;;; > (headers:header-utilities)
