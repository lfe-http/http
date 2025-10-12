# Phases 4-5-6: Response Builder, Status Codes, & Erlang Interop

## Overview
These three phases can be developed sequentially but are grouped together as they're interdependent. Phase 4 builds the response system, Phase 5 enhances status codes, and Phase 6 optimizes the critical Erlang httpc interop layer.

**Combined Estimated Time**: 6-9 hours  
**Dependencies**: All previous phases

---

# Phase 4: Response Builder (2-3 hours)

## Objectives

1. ✅ Optimize response construction with direct map building
2. ✅ Add convenience response builders (ok, error, json, etc.)
3. ✅ Streamline body setting with single-pass conversion
4. ✅ Maintain backward compatibility

---

## Implementation

### File: `src/http.response.lfe`

```lfe
(defmodule http.response
  (export
   ;; Constructors
   (new 0) (new 1) (new 2) (new 3)
   
   ;; Setters
   (set-status 2)
   (set-body 2)
   (set-headers 2)
   (set-header 3)
   (add-header 3)
   (remove-header 2)
   
   ;; Getters
   (status 1)
   (body 1)
   (headers 1)
   
   ;; Convenience builders
   (ok 0) (ok 1)
   (created 0) (created 1)
   (accepted 0) (accepted 1)
   (no-content 0)
   (bad-request 0) (bad-request 1)
   (unauthorized 0) (unauthorized 1)
   (forbidden 0) (forbidden 1)
   (not-found 0) (not-found 1)
   (error 0) (error 1)
   (bad-gateway 0) (bad-gateway 1)
   (unavailable 0) (unavailable 1)
   
   ;; Content-type helpers
   (json 2)
   (text 2)
   (html 2)
   (xml 2)))

;; Compiler directives
(compile (inline status 1))
(compile (inline body 1))
(compile (inline headers 1))
(compile (inline ensure-binary 1))

;;; ---------------------------------------------------------------------------
;;; Constructors
;;; ---------------------------------------------------------------------------

(defun new
  "Create a new empty response with 200 OK.
  
  Returns:
    Response map with empty body"
  ()
  #m(status 200
     headers #m()
     body #""
     version 1.1))

(defun new
  "Create response with status code.
  
  Args:
    status: HTTP status code (integer)
    
  Returns:
    Response map"
  ((status) (when (is_integer status))
   (mset (new) 'status status)))

(defun new
  "Create response with status and body.
  
  Args:
    status: HTTP status code
    body: Response body
    
  Returns:
    Response map"
  ((status body)
   (-> (new)
       (mset 'status status)
       (mset 'body (ensure-binary body)))))

(defun new
  "Create response with status, headers, and body.
  
  Args:
    status: HTTP status code
    headers: Headers map
    body: Response body
    
  Returns:
    Response map"
  ((status headers body)
   #m(status status
      headers headers
      body (ensure-binary body)
      version 1.1)))

;;; ---------------------------------------------------------------------------
;;; Setters
;;; ---------------------------------------------------------------------------

(defun set-status
  "Set the HTTP status code.
  
  Args:
    resp: Response map
    status: HTTP status code (integer)
    
  Returns:
    Updated response map"
  ((resp status) (when (is_integer status))
   (mset resp 'status status)))

(defun set-body
  "Set the response body.
  
  Args:
    resp: Response map
    body: Body content (will be converted to binary)
    
  Returns:
    Updated response map"
  ((resp body)
   (mset resp 'body (ensure-binary body))))

(defun set-headers
  "Replace all headers.
  
  Args:
    resp: Response map
    headers: New headers map
    
  Returns:
    Updated response map"
  ((resp headers) (when (is_map headers))
   (mset resp 'headers headers)))

(defun set-header
  "Set a single header.
  
  Args:
    resp: Response map
    key: Header name
    val: Header value
    
  Returns:
    Updated response map"
  ((resp key val)
   (let* ((hs (mref resp 'headers))
          (hs2 (http.header:add hs key val)))
     (mset resp 'headers hs2))))

(defun add-header
  "Add a header (alias for set-header).
  
  Args:
    resp: Response map
    key: Header name
    val: Header value
    
  Returns:
    Updated response map"
  ((resp key val)
   (set-header resp key val)))

(defun remove-header
  "Remove a header.
  
  Args:
    resp: Response map
    key: Header name
    
  Returns:
    Updated response map"
  ((resp key)
   (let* ((hs (mref resp 'headers))
          (hs2 (http.header:remove hs key)))
     (mset resp 'headers hs2))))

;;; ---------------------------------------------------------------------------
;;; Getters
;;; ---------------------------------------------------------------------------

(defun status ((resp) (mref resp 'status)))
(defun body ((resp) (mref resp 'body)))
(defun headers ((resp) (mref resp 'headers)))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (2xx Success)
;;; ---------------------------------------------------------------------------

(defun ok () (new 200))
(defun ok (body) (new 200 body))

(defun created () (new 201))
(defun created (body) (new 201 body))

(defun accepted () (new 202))
(defun accepted (body) (new 202 body))

(defun no-content () (new 204))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (4xx Client Error)
;;; ---------------------------------------------------------------------------

(defun bad-request () (new 400))
(defun bad-request (body) (new 400 body))

(defun unauthorized () (new 401))
(defun unauthorized (body) (new 401 body))

(defun forbidden () (new 403))
(defun forbidden (body) (new 403 body))

(defun not-found () (new 404))
(defun not-found (body) (new 404 body))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (5xx Server Error)
;;; ---------------------------------------------------------------------------

(defun error () (new 500))
(defun error (body) (new 500 body))

(defun bad-gateway () (new 502))
(defun bad-gateway (body) (new 502 body))

(defun unavailable () (new 503))
(defun unavailable (body) (new 503 body))

;;; ---------------------------------------------------------------------------
;;; Content-Type Helpers
;;; ---------------------------------------------------------------------------

(defun json
  "Create JSON response with status and data.
  
  Args:
    status: HTTP status code
    json-body: JSON body (already encoded as binary)
    
  Returns:
    Response with JSON content-type"
  ((status json-body)
   (-> (new status json-body)
       (set-header #"Content-Type" #"application/json; charset=utf-8"))))

(defun text
  "Create plain text response.
  
  Args:
    status: HTTP status code
    text-body: Text body
    
  Returns:
    Response with text content-type"
  ((status text-body)
   (-> (new status text-body)
       (set-header #"Content-Type" #"text/plain; charset=utf-8"))))

(defun html
  "Create HTML response.
  
  Args:
    status: HTTP status code
    html-body: HTML body
    
  Returns:
    Response with HTML content-type"
  ((status html-body)
   (-> (new status html-body)
       (set-header #"Content-Type" #"text/html; charset=utf-8"))))

(defun xml
  "Create XML response.
  
  Args:
    status: HTTP status code
    xml-body: XML body
    
  Returns:
    Response with XML content-type"
  ((status xml-body)
   (-> (new status xml-body)
       (set-header #"Content-Type" #"application/xml; charset=utf-8"))))

;;; ---------------------------------------------------------------------------
;;; Private Helpers
;;; ---------------------------------------------------------------------------

(defun ensure-binary
  "Convert value to binary (inline-optimized).
  
  Args:
    val: Value to convert
    
  Returns:
    Binary representation"
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (iolist_to_binary l))
  ((a) (when (is_atom a)) (atom_to_binary a))
  ((i) (when (is_integer i)) (integer_to_binary i)))
```

---

# Phase 5: Status Code Enhancement (1-2 hours)

## Objectives

1. ✅ Add status code macros for compile-time optimization
2. ✅ Implement validation and lookup functions
3. ✅ Keep existing function API for compatibility
4. ✅ Optional: Move novelty 7xx codes to separate module

---

## Implementation

### File: `src/http.status.lfe`

```lfe
(defmodule http.status
  (export all)) ; Keep all existing exports

;;; ---------------------------------------------------------------------------
;;; Status Code Macros (Compile-Time Constants)
;;; ---------------------------------------------------------------------------

;; 2xx Success
(defmacro OK () 200)
(defmacro CREATED () 201)
(defmacro ACCEPTED () 202)
(defmacro NO-CONTENT () 204)

;; 3xx Redirection
(defmacro MOVED-PERMANENTLY () 301)
(defmacro FOUND () 302)
(defmacro SEE-OTHER () 303)
(defmacro NOT-MODIFIED () 304)
(defmacro TEMPORARY-REDIRECT () 307)
(defmacro PERMANENT-REDIRECT () 308)

;; 4xx Client Error
(defmacro BAD-REQUEST () 400)
(defmacro UNAUTHORIZED () 401)
(defmacro FORBIDDEN () 403)
(defmacro NOT-FOUND () 404)
(defmacro METHOD-NOT-ALLOWED () 405)
(defmacro CONFLICT () 409)
(defmacro GONE () 410)
(defmacro TOO-MANY-REQUESTS () 429)

;; 5xx Server Error
(defmacro INTERNAL-SERVER-ERROR () 500)
(defmacro NOT-IMPLEMENTED () 501)
(defmacro BAD-GATEWAY () 502)
(defmacro SERVICE-UNAVAILABLE () 503)
(defmacro GATEWAY-TIMEOUT () 504)

;;; ---------------------------------------------------------------------------
;;; Validation and Lookup Functions
;;; ---------------------------------------------------------------------------

(defun valid?
  "Check if status code is valid (100-599).
  
  Args:
    code: Integer status code
    
  Returns:
    Boolean true/false"
  ((code) (when (is_integer code))
   (and (>= code 100) (<= code 599)))
  ((_) 'false))

(defun reason-phrase
  "Get reason phrase for status code.
  
  Args:
    code: Integer status code
    
  Returns:
    Binary reason phrase or #\"Unknown\""
  ((code) (when (is_integer code))
   (maps:get code (reason-phrase-map) #"Unknown")))

(defun reason-phrase-map
  "Return map of status codes to reason phrases.
  
  Returns:
    Map of integer codes to binary phrases"
  ()
  #m(;; 1xx Informational
     100 #"Continue"
     101 #"Switching Protocols"
     102 #"Processing"
     
     ;; 2xx Success
     200 #"OK"
     201 #"Created"
     202 #"Accepted"
     203 #"Non-Authoritative Information"
     204 #"No Content"
     205 #"Reset Content"
     206 #"Partial Content"
     207 #"Multi-Status"
     208 #"Already Reported"
     226 #"IM Used"
     
     ;; 3xx Redirection
     300 #"Multiple Choices"
     301 #"Moved Permanently"
     302 #"Found"
     303 #"See Other"
     304 #"Not Modified"
     305 #"Use Proxy"
     307 #"Temporary Redirect"
     308 #"Permanent Redirect"
     
     ;; 4xx Client Error
     400 #"Bad Request"
     401 #"Unauthorized"
     402 #"Payment Required"
     403 #"Forbidden"
     404 #"Not Found"
     405 #"Method Not Allowed"
     406 #"Not Acceptable"
     407 #"Proxy Authentication Required"
     408 #"Request Timeout"
     409 #"Conflict"
     410 #"Gone"
     411 #"Length Required"
     412 #"Precondition Failed"
     413 #"Payload Too Large"
     414 #"URI Too Long"
     415 #"Unsupported Media Type"
     416 #"Range Not Satisfiable"
     417 #"Expectation Failed"
     418 #"I'm a teapot"
     422 #"Unprocessable Entity"
     423 #"Locked"
     424 #"Failed Dependency"
     426 #"Upgrade Required"
     428 #"Precondition Required"
     429 #"Too Many Requests"
     431 #"Request Header Fields Too Large"
     451 #"Unavailable For Legal Reasons"
     
     ;; 5xx Server Error
     500 #"Internal Server Error"
     501 #"Not Implemented"
     502 #"Bad Gateway"
     503 #"Service Unavailable"
     504 #"Gateway Timeout"
     505 #"HTTP Version Not Supported"
     506 #"Variant Also Negotiates"
     507 #"Insufficient Storage"
     508 #"Loop Detected"
     510 #"Not Extended"
     511 #"Network Authentication Required"))

;;; ---------------------------------------------------------------------------
;;; Keep ALL existing function-based status codes for compatibility
;;; (Include all from original http.status.lfe)
;;; ---------------------------------------------------------------------------

; HTTP 1xx
(defun continue () 100)
(defun switching-protocols () 101)
(defun processing () 102)

; HTTP 2xx
(defun ok () 200)
(defun created () 201)
(defun accepted () 202)
(defun non-authoritative-information () 203)
(defun no-content () 204)
(defun reset-content () 205)
(defun partial-content () 206)
(defun multi-status () 207)
(defun already-reported () 208)
(defun im-used () 226)

; HTTP 3xx
(defun multiple-choices () 300)
(defun moved-permanently () 301)
(defun found () 302)
(defun see-other () 303)
(defun not-modified () 304)
(defun use-proxy () 305)
(defun switch-proxy () 306)
(defun temporary-redirect () 307)
(defun permanent-redirect () 308)

; HTTP 4xx
(defun bad-request () 400)
(defun unauthorized () 401)
(defun payment-required () 402)
(defun forbidden () 403)
(defun not-found () 404)
(defun method-not-allowed () 405)
(defun not-acceptable () 406)
(defun proxy-authentication-required () 407)
(defun request-timeout () 408)
(defun conflict () 409)
(defun gone () 410)
(defun length-required () 411)
(defun precondition-failed () 412)
(defun request-entity-too-large () 413)
(defun request-uri-too-long () 414)
(defun unsupported-media-type () 415)
(defun requested-range-not-satisfiable () 416)
(defun expectation-failed () 417)
(defun im-a-teapot () 418)
(defun authentication-timeout () 419)
(defun method-failure () 420)
(defun unprocessable-entity () 422)
(defun locked () 423)
(defun unordered-collection () 425)
(defun upgrade-required () 426)
(defun precondition-required () 428)
(defun too-many-requests () 429)
(defun request-header-fields-too-large () 431)
(defun no-response () 444)
(defun retry-with () 449)
(defun blocked-by-windows-parental-controls () 450)
(defun unavailable-for-legal-reasons () 451)
(defun request-header-too-large () 494)
(defun cert-error () 495)
(defun no-cert () 496)
(defun http-to-https () 497)
(defun client-closed-request () 499)

; HTTP 5xx
(defun internal-server-error () 500)
(defun not-implemented () 501)
(defun bad-gateway () 502)
(defun service-unavailable () 503)
(defun gateway-timeout () 504)
(defun http-version-not-supported () 505)
(defun variant-also-negotiates () 506)
(defun insufficient-storage () 507)
(defun loop-detected () 508)
(defun bandwidth-limit-exceeded () 509)
(defun not-extended () 510)
(defun network-authentication-required () 511)
(defun connection-timed-out () 522)
(defun network-read-timeout-error () 598)
(defun network-connect-timeout-error () 599)

; HTTP 70x-79x - Keep all novelty codes for backward compatibility
; (Include all from original - omitted here for brevity, but MUST be in actual file)
```

---

# Phase 6: Erlang httpc Interop (3-4 hours)

## Objectives

1. ✅ Replace atom method dispatch with binary pattern matching
2. ✅ Optimize conversion functions (single-pass)
3. ✅ Reduce intermediate allocations
4. ✅ Achieve 30-40% performance improvement

---

## Implementation

### File: `src/http.c.lfe`

```lfe
;;;; This module provides interoperability between the LFE HTTP library and the
;;;; Erlang stdlib httpc HTTP client library.
(defmodule http.c
  (export
   (->erlang 1) (->erlang 3)
   (erlang-> 1)
   (request 1) (request 2) (request 3) (request 4) (request 5) (request 6)))

;; Compiler directives
(compile (inline method->atom 1))

;;; ---------------------------------------------------------------------------
;;; Public Request Functions
;;; ---------------------------------------------------------------------------

(defun request
  "Make HTTP request with URL only (GET request).
  
  Args:
    url: Request URL (string or binary)
    
  Returns:
    #(ok response) or error tuple"
  ((url)
   (request-internal (http.request:new url))))

(defun request
  "Make HTTP request with method and URL.
  
  Args:
    method: HTTP method (binary, atom, or string)
    url: Request URL
    
  Returns:
    #(ok response) or error tuple"
  ((method url)
   (request-internal (http.request:new method url))))

(defun request
  "Make HTTP request with method, URL, and body.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    
  Returns:
    #(ok response) or error tuple"
  ((method url body)
   (request-internal (http.request:new method url body))))

(defun request
  "Make HTTP request with method, URL, body, and headers.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map
    
  Returns:
    #(ok response) or error tuple"
  ((method url body headers)
   (request-internal (http.request:new method url body headers))))

(defun request
  "Make HTTP request with HTTP options.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options
    
  Returns:
    #(ok response) or error tuple"
  ((method url body http-options options)
   (let ((req (http.request:new method url body)))
     (request-internal req http-options options))))

(defun request
  "Make HTTP request with headers and HTTP options.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options
    
  Returns:
    #(ok response) or error tuple"
  ((method url body headers http-options options)
   (let ((req (http.request:new method url body headers)))
     (request-internal req http-options options))))

;;; ---------------------------------------------------------------------------
;;; Internal Request Functions
;;; ---------------------------------------------------------------------------

(defun request-internal
  "Internal request with default options.
  
  Args:
    req: LFE HTTP request map
    
  Returns:
    #(ok response) or error tuple"
  ((req)
   (request-internal req '() '())))

(defun request-internal
  "Internal request with HTTP options.
  
  Args:
    req: LFE HTTP request map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options
    
  Returns:
    #(ok response) or error tuple"
  ((req http-options options)
   (let* ((version (mref req 'version))
          (version-tuple (http.util:http-version-tuple version))
          ;; Merge version into http-options
          (http-opts (cons `#(version ,version-tuple) http-options))
          ;; Ensure sync and full_result
          (opts (lists:append options '(#(sync true) #(full_result true))))
          ;; Convert to Erlang format
          (args (->erlang req http-opts opts)))
     (case (apply #'httpc:request/4 args)
       (`#(ok ,httpc-resp) `#(ok ,(erlang-> httpc-resp)))
       (err err)))))

;;; ---------------------------------------------------------------------------
;;; LFE -> Erlang Conversion
;;; ---------------------------------------------------------------------------

(defun ->erlang
  "Convert LFE HTTP request to Erlang httpc format with default options.
  
  Args:
    req: LFE HTTP request map
    
  Returns:
    List of arguments for httpc:request/4"
  ((req)
   (->erlang req
             `(#(version ,(http.util:http-version-tuple (mref req 'version))))
             '(#(sync true) #(full_result true)))))

(defun ->erlang
  "Convert LFE HTTP request to Erlang httpc format.
  
  Args:
    req: LFE HTTP request map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options
    
  Returns:
    List of arguments for httpc:request/4"
  ((req http-options options)
   (->erlang-dispatch (mref req 'method) req http-options options)))

;;; ---------------------------------------------------------------------------
;;; Binary Method Dispatch (Optimized)
;;; ---------------------------------------------------------------------------

(defun ->erlang-dispatch
  "Dispatch on binary HTTP method for optimized conversion.
  Uses binary pattern matching instead of atom comparison.
  
  Args:
    method: Binary HTTP method (e.g., #\"GET\")
    req: Request map
    http-options: HTTP options
    options: Options
    
  Returns:
    List of arguments for httpc:request/4"
  ;; Methods without body
  ((#"GET" req http-opts opts)
   (->erlang-no-body 'get req http-opts opts))
  ((#"HEAD" req http-opts opts)
   (->erlang-no-body 'head req http-opts opts))
  ((#"DELETE" req http-opts opts)
   (->erlang-no-body 'delete req http-opts opts))
  ((#"OPTIONS" req http-opts opts)
   (->erlang-no-body 'options req http-opts opts))
  ((#"TRACE" req http-opts opts)
   (->erlang-no-body 'trace req http-opts opts))
  
  ;; Methods with body
  ((#"POST" req http-opts opts)
   (->erlang-with-body 'post req http-opts opts))
  ((#"PUT" req http-opts opts)
   (->erlang-with-body 'put req http-opts opts))
  ((#"PATCH" req http-opts opts)
   (->erlang-with-body 'patch req http-opts opts))
  
  ;; Fallback for custom methods
  ((method req http-opts opts)
   (let ((method-atom (method->atom method)))
     (if (http:method-has-body? method)
       (->erlang-with-body method-atom req http-opts opts)
       (->erlang-no-body method-atom req http-opts opts)))))

;;; ---------------------------------------------------------------------------
;;; Conversion Helpers
;;; ---------------------------------------------------------------------------

(defun ->erlang-no-body
  "Convert request without body to Erlang format.
  Optimized for single-pass conversion.
  
  Args:
    method-atom: Erlang method atom
    req: Request map
    http-options: HTTP options
    options: Options
    
  Returns:
    List #(method url headers) http-options options"
  ((method-atom req http-opts opts)
   (let* ((url (mref req 'url))
          (headers (mref req 'headers))
          (header-list (maps:to_list headers)))
     `(,method-atom
       #(,url ,header-list)
       ,http-opts
       ,opts))))

(defun ->erlang-with-body
  "Convert request with body to Erlang format.
  Optimized for single-pass conversion.
  
  Args:
    method-atom: Erlang method atom
    req: Request map
    http-options: HTTP options
    options: Options
    
  Returns:
    List #(method url headers content-type body) http-options options"
  ((method-atom req http-opts opts)
   (let* ((url (mref req 'url))
          (headers (mref req 'headers))
          (header-list (maps:to_list headers))
          (body (mref req 'body))
          (content-type 
           (http.header:get-ci headers #"Content-Type" 
                               #"application/octet-stream")))
     `(,method-atom
       #(,url
         ,header-list
         ,(binary_to_list content-type)
         ,body)
       ,http-opts
       ,(cons #(body_format binary) opts)))))

(defun method->atom
  "Convert binary method to lowercase atom for Erlang httpc.
  Inline-optimized.
  
  Args:
    method: Binary HTTP method
    
  Returns:
    Lowercase atom"
  ((method) (when (is_binary method))
   (http.util:binary-downcase-atom method)))

;;; ---------------------------------------------------------------------------
;;; Erlang -> LFE Conversion
;;; ---------------------------------------------------------------------------

(defun erlang->
  "Convert Erlang httpc response to LFE HTTP response.
  Single-pass conversion with optimized header handling.
  
  Args:
    httpc-resp: Erlang httpc response tuple
    
  Returns:
    LFE HTTP response map"
  ((httpc-resp)
   (let ((`#(#(,_version ,status-code ,_reason-phrase) ,header-list ,body-data) 
          httpc-resp))
     ;; Direct map construction - single pass
     #m(status status-code
        headers (http.header:from-list header-list)
        body (iolist_to_binary body-data)
        version 1.1))))
```

---

## Testing Requirements

### Test File: `test/http-response-tests.lfe`

```lfe
(defmodule http-response-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun new-test_ ()
  (list
   ;; Empty response
   (let ((r (http.response:new)))
     (?_assertEqual 200 (mref r 'status)))
   
   ;; With status
   (let ((r (http.response:new 404)))
     (?_assertEqual 404 (mref r 'status)))
   
   ;; With status and body
   (let ((r (http.response:new 200 #"OK")))
     (?_assertEqual #"OK" (mref r 'body)))))

(defun convenience-builders-test_ ()
  (list
   (?_assertEqual 200 (mref (http.response:ok) 'status))
   (?_assertEqual 201 (mref (http.response:created) 'status))
   (?_assertEqual 204 (mref (http.response:no-content) 'status))
   (?_assertEqual 400 (mref (http.response:bad-request) 'status))
   (?_assertEqual 404 (mref (http.response:not-found) 'status))
   (?_assertEqual 500 (mref (http.response:error) 'status))))

(defun json-helper-test ()
  (let ((r (http.response:json 200 #"{\"key\":\"value\"}")))
    (?_assertEqual #"application/json; charset=utf-8"
                   (http.header:get (mref r 'headers) #"Content-Type"))))
```

### Test File: `test/http-c-tests.lfe`

```lfe
(defmodule http-c-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun erlang-conversion-test ()
  ;; Test ->erlang conversion
  (let* ((req (http.request:new #"GET" "http://example.com"))
         (args (http.c:->erlang req))
         (`(,method ,request ,_http-opts ,_opts) args))
    (?_assertEqual 'get method)
    (let ((`#(,url ,_headers) request))
      (?_assertEqual #"http://example.com" url))))

(defun method-dispatch-test_ ()
  (list
   ;; GET (no body)
   (let* ((req (http.request:new #"GET" "http://example.com"))
          (`(,method #(,_ ,_) ,_ ,_) (http.c:->erlang req)))
     (?_assertEqual 'get method))
   
   ;; POST (with body)
   (let* ((req (http.request:new #"POST" "http://example.com" #"body"))
          (`(,method #(,_ ,_ ,_ ,_) ,_ ,_) (http.c:->erlang req)))
     (?_assertEqual 'post method))))
```

---

## Benchmarking Requirements

### Benchmark File: `bench/interop-bench.lfe`

```lfe
(defmodule interop-bench
  (export (run 0)))

(defun run ()
  (io:format "~n=== HTTP Interop Benchmarks ===~n")
  
  ;; Benchmark ->erlang conversion
  (let* ((req (http.request:new #"POST" "http://example.com/api" 
                                #"body" #m(#"X-Custom" #"value")))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.c:->erlang req))
                (lists:seq 1 iterations))))))
    (io:format "  ->erlang: ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))
  
  ;; Benchmark erlang-> conversion
  (let* ((httpc-resp `#(#(#(1 1) 200 #"OK")
                        ((#"content-type" "text/html")
                         (#"content-length" "123"))
                        #"response body"))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.c:erlang-> httpc-resp))
                (lists:seq 1 iterations))))))
    (io:format "  erlang->: ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
```

---

## Completion Checklist

### Phase 4
- [ ] `http.response.lfe` fully implemented
- [ ] Convenience builders working
- [ ] Content-type helpers working
- [ ] All tests pass

### Phase 5
- [ ] Status code macros added
- [ ] Validation functions working
- [ ] Reason phrase lookup working
- [ ] All existing functions maintained

### Phase 6
- [ ] Binary method dispatch implemented
- [ ] Single-pass conversions working
- [ ] All request variants work
- [ ] Performance improvement ≥30%

---

## Performance Expectations

### Phase 4 (Response)
- new/3: 50-100ns per operation
- Convenience builders: 80-150ns
- json/2: 200-400ns (includes header update)

### Phase 6 (Interop)
- ->erlang: 1-3μs per operation (30-40% faster than v0.5.4)
- erlang->: 2-5μs per operation
- Full request cycle: 25-35% overall improvement

---

## Next Steps

After completing Phases 4-5-6:
1. Commit: "Phases 4-5-6: Response, Status, Interop complete"
2. Tag: `v1.0.0-phase4-5-6`
3. Proceed to **Phase 7: Testing & Benchmarking**