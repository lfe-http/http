# Phase 3: Request Builder

## Overview
Rewrite the request building module to eliminate multiple `maps:merge` calls, reduce allocations, and provide efficient multi-arity constructors. This phase focuses on single-pass request construction with minimal memory overhead.

**Estimated Time**: 3-4 hours  
**Dependencies**: Phase 1 (http.util, http), Phase 2 (http.header)  
**Modules**: `http.request.lfe`

---

## Objectives

1. ✅ Replace multiple `maps:merge` with direct map construction
2. ✅ Remove `->list` debug function (or move to separate module)
3. ✅ Implement efficient multi-arity constructors
4. ✅ Add threading-style helper functions
5. ✅ Optimize URL parsing (single pass)
6. ✅ Achieve 40-60% reduction in allocations

---

## Current Implementation Analysis

### Current State (v0.5.4)
```lfe
(defmodule http.request
  (export
   (->list 1)
   (add-header 3)
   (new 1) (new 2) (new 3) (new 4)))

(defun new
  ((url) (when (is_list url))
   (new (list_to_binary url)))
  ((url)
   (let ((parsed-url (yuri:parse url)))
     `#m(method get
         version ,(http:default-version)
         remote-addr #""
         headers ,(http:default-headers)
         body #""
         url ,url
         url-parsed ,(maps:merge parsed-url (yuri.user:parse parsed-url))
         path-segments ()
         query-parsed #m()))))

(defun new (method url)
  (new method url #""))

(defun new (method url body)
  (new method url body (http:default-headers)))

(defun new (method url body headers)
  (let* ((init (new url))
         (url (mref init 'url-parsed)))
    (maps:merge init
                `#m(method ,method
                    body ,body
                    headers ,headers
                    path-segments ,(yuri.path:->segments (mref url 'path))
                    query-parsed ,(yuri.query:parse (mref url 'query))))))

(defun ->list (req)
  (let* ((h (maps:get 'headers req #m()))
         (q (maps:get 'query-parsed req #m()))
         (u (maps:get 'url-parsed req #m()))
         (req2 (maps:merge req `#m(headers ,(lists:sort (maps:to_list h))
                                   query-parsed ,(lists:sort (maps:to_list q))
                                   url-parsed ,(lists:sort (maps:to_list u))))))
    (lists:sort (maps:to_list req2))))

(defun add-header (req k v)
  (http.util:add-header req k v))
```

### Problems Identified
1. **Multiple merges**: `new/4` does `maps:merge` on already-constructed map
2. **Redundant parsing**: Parses URL, then re-parses segments and query
3. **`->list` overhead**: Only for debugging, creates sorted lists
4. **Atom methods**: Still using atom `'get` instead of binary
5. **No builder pattern**: Can't chain operations efficiently

---

## New Implementation

### File: `src/http.request.lfe`

```lfe
(defmodule http.request
  (export
   ;; Constructors
   (new 1) (new 2) (new 3) (new 4)
   
   ;; Setters (builder pattern)
   (set-method 2)
   (set-body 2)
   (set-headers 2)
   (set-header 3)
   (add-header 3)
   (remove-header 2)
   
   ;; Getters
   (method 1)
   (url 1)
   (body 1)
   (headers 1)
   (path-segments 1)
   (query-params 1)
   
   ;; Content-type helpers
   (set-json 2)
   (set-form 2)
   (set-text 2)
   
   ;; Query parameter helpers
   (add-query-param 3)
   (set-query-params 2)
   
   ;; Debugging (optional, for development)
   (to-map 1)))

;; Compiler directives
(compile (inline ensure-method-binary 1))
(compile (inline method 1))
(compile (inline url 1))
(compile (inline body 1))
(compile (inline headers 1))

;;; ---------------------------------------------------------------------------
;;; Constructors (Optimized for Single-Pass Construction)
;;; ---------------------------------------------------------------------------

(defun new
  "Create a new HTTP request with URL.
  Single-pass construction - all parsing done once.
  
  Args:
    url: Request URL (binary or string)
    
  Returns:
    Request map with GET method and default headers"
  ((url) (when (is_list url))
   (new (list_to_binary url)))
  ((url) (when (is_binary url))
   (let* ((parsed (yuri:parse url))
          (parsed-full (maps:merge parsed (yuri.user:parse parsed)))
          (path (maps:get 'path parsed-full #""))
          (query (maps:get 'query parsed-full #"")))
     ;; Direct map construction - no merges
     #m(method #"GET"
        version 1.1
        remote-addr #""
        headers (http:default-headers)
        body #""
        url url
        url-parsed parsed-full
        path-segments (yuri.path:->segments path)
        query-parsed (yuri.query:parse query)))))

(defun new
  "Create request with method and URL.
  
  Args:
    method: HTTP method (binary, atom, or string)
    url: Request URL
    
  Returns:
    Request map"
  ((method url)
   (let ((req (new url)))
     (mset req 'method (ensure-method-binary method)))))

(defun new
  "Create request with method, URL, and body.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body (binary, string, or iolist)
    
  Returns:
    Request map"
  ((method url body)
   (let ((req (new method url)))
     (mset req 'body (http.util:ensure-binary body)))))

(defun new
  "Create request with method, URL, body, and headers.
  
  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map
    
  Returns:
    Request map"
  ((method url body headers)
   (let* ((req (new method url body))
          (req2 (mset req 'headers headers)))
     req2)))

;;; ---------------------------------------------------------------------------
;;; Setters (Builder Pattern)
;;; ---------------------------------------------------------------------------

(defun set-method
  "Set the HTTP method.
  
  Args:
    req: Request map
    method: HTTP method (will be converted to uppercase binary)
    
  Returns:
    Updated request map"
  ((req method)
   (mset req 'method (ensure-method-binary method))))

(defun set-body
  "Set the request body.
  
  Args:
    req: Request map
    body: Body content (binary, string, iolist, atom, or integer)
    
  Returns:
    Updated request map"
  ((req body)
   (mset req 'body (http.util:ensure-binary body))))

(defun set-headers
  "Replace all headers.
  
  Args:
    req: Request map
    headers: New headers map
    
  Returns:
    Updated request map"
  ((req headers) (when (is_map headers))
   (mset req 'headers headers)))

(defun set-header
  "Set a single header (replaces if exists).
  
  Args:
    req: Request map
    key: Header name
    val: Header value
    
  Returns:
    Updated request map"
  ((req key val)
   (let* ((hs (mref req 'headers))
          (hs2 (http.header:add hs key val)))
     (mset req 'headers hs2))))

(defun add-header
  "Add a header (alias for set-header for compatibility).
  
  Args:
    req: Request map
    key: Header name
    val: Header value
    
  Returns:
    Updated request map"
  ((req key val)
   (set-header req key val)))

(defun remove-header
  "Remove a header.
  
  Args:
    req: Request map
    key: Header name to remove
    
  Returns:
    Updated request map"
  ((req key)
   (let* ((hs (mref req 'headers))
          (hs2 (http.header:remove hs key)))
     (mset req 'headers hs2))))

;;; ---------------------------------------------------------------------------
;;; Getters (Inline-Optimized)
;;; ---------------------------------------------------------------------------

(defun method
  "Get HTTP method from request.
  
  Args:
    req: Request map
    
  Returns:
    Binary HTTP method"
  ((req) (mref req 'method)))

(defun url
  "Get URL from request.
  
  Args:
    req: Request map
    
  Returns:
    Binary URL"
  ((req) (mref req 'url)))

(defun body
  "Get body from request.
  
  Args:
    req: Request map
    
  Returns:
    Binary body"
  ((req) (mref req 'body)))

(defun headers
  "Get headers from request.
  
  Args:
    req: Request map
    
  Returns:
    Headers map"
  ((req) (mref req 'headers)))

(defun path-segments
  "Get URL path segments.
  
  Args:
    req: Request map
    
  Returns:
    List of binary path segments"
  ((req) (mref req 'path-segments)))

(defun query-params
  "Get query parameters.
  
  Args:
    req: Request map
    
  Returns:
    Map of query parameters"
  ((req) (mref req 'query-parsed)))

;;; ---------------------------------------------------------------------------
;;; Content-Type Helpers
;;; ---------------------------------------------------------------------------

(defun set-json
  "Set body and Content-Type for JSON.
  
  Args:
    req: Request map
    json-body: JSON body (already encoded as binary)
    
  Returns:
    Updated request map with JSON content-type"
  ((req json-body)
   (-> req
       (set-body json-body)
       (set-header #"Content-Type" #"application/json; charset=utf-8"))))

(defun set-form
  "Set body and Content-Type for form data.
  
  Args:
    req: Request map
    form-data: URL-encoded form data (binary) or params map
    
  Returns:
    Updated request map with form content-type"
  ((req form-data) (when (is_map form-data))
   (set-form req (http.util:query-string form-data)))
  ((req form-data) (when (is_binary form-data))
   (-> req
       (set-body form-data)
       (set-header #"Content-Type" #"application/x-www-form-urlencoded"))))

(defun set-text
  "Set body and Content-Type for plain text.
  
  Args:
    req: Request map
    text-body: Text body (binary or string)
    
  Returns:
    Updated request map with text content-type"
  ((req text-body)
   (-> req
       (set-body text-body)
       (set-header #"Content-Type" #"text/plain; charset=utf-8"))))

;;; ---------------------------------------------------------------------------
;;; Query Parameter Helpers
;;; ---------------------------------------------------------------------------

(defun add-query-param
  "Add a query parameter to the request.
  Modifies both query-parsed map and reconstructs URL.
  
  Args:
    req: Request map
    key: Parameter name (will be converted to binary)
    val: Parameter value (will be converted to binary)
    
  Returns:
    Updated request map with new query parameter"
  ((req key val)
   (let* ((query-map (mref req 'query-parsed))
          (key-bin (http.util:ensure-binary key))
          (val-bin (http.util:ensure-binary val))
          (updated-query (maps:put key-bin val-bin query-map))
          (url-parsed (mref req 'url-parsed))
          (new-query-string (http.util:query-string updated-query))
          ;; Reconstruct URL with new query
          (base-url (binary-without-query (mref req 'url)))
          (new-url (if (== new-query-string #"")
                     base-url
                     (iolist_to_binary (list base-url #"?" new-query-string)))))
     (-> req
         (mset 'query-parsed updated-query)
         (mset 'url new-url)
         (mset 'url-parsed (maps:put 'query new-query-string url-parsed))))))

(defun set-query-params
  "Replace all query parameters.
  
  Args:
    req: Request map
    params: Map of query parameters
    
  Returns:
    Updated request map"
  ((req params) (when (is_map params))
   (let* ((query-string (http.util:query-string params))
          (base-url (binary-without-query (mref req 'url)))
          (new-url (if (== query-string #"")
                     base-url
                     (iolist_to_binary (list base-url #"?" query-string))))
          (url-parsed (mref req 'url-parsed)))
     (-> req
         (mset 'query-parsed params)
         (mset 'url new-url)
         (mset 'url-parsed (maps:put 'query query-string url-parsed))))))

;;; ---------------------------------------------------------------------------
;;; Private Helper Functions
;;; ---------------------------------------------------------------------------

(defun ensure-method-binary
  "Convert method to uppercase binary.
  Inline-optimized for hot-path usage.
  
  Args:
    method: HTTP method (binary, atom, or string)
    
  Returns:
    Uppercase binary method"
  ((method) (when (is_binary method))
   (http.util:binary-upcase method))
  ((method) (when (is_atom method))
   (http.util:binary-upcase (atom_to_binary method)))
  ((method) (when (is_list method))
   (http.util:binary-upcase (list_to_binary method))))

(defun binary-without-query
  "Remove query string from URL.
  
  Args:
    url: Binary URL
    
  Returns:
    URL without query string"
  ((url) (when (is_binary url))
   (case (binary:split url #"?")
     (`(,base ,_) base)
     (`(,base) base))))

;;; ---------------------------------------------------------------------------
;;; Debugging Utilities
;;; ---------------------------------------------------------------------------

(defun to-map
  "Convert request to sorted map representation for debugging.
  This is only for development/debugging - not optimized.
  
  Args:
    req: Request map
    
  Returns:
    Request map with sorted nested structures"
  ((req) (when (is_map req))
   (let* ((h (maps:get 'headers req #m()))
          (q (maps:get 'query-parsed req #m()))
          (u (maps:get 'url-parsed req #m())))
     (maps:merge req
                 `#m(headers ,(maps:from_list (lists:sort (maps:to_list h)))
                     query-parsed ,(maps:from_list (lists:sort (maps:to_list q)))
                     url-parsed ,(maps:from_list (lists:sort (maps:to_list u))))))))
```

---

## Testing Requirements

### Test File: `test/http-request-tests.lfe`

```lfe
(defmodule http-request-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

;;; Constructor tests
(defun new-url-only-test ()
  (let ((req (http.request:new "http://example.com/path")))
    (?_assertEqual #"GET" (mref req 'method))
    (?_assertEqual #"http://example.com/path" (mref req 'url))
    (?_assertEqual #"" (mref req 'body))
    (?_assert (is_map (mref req 'headers)))))

(defun new-with-method-test ()
  (let ((req (http.request:new #"POST" "http://example.com")))
    (?_assertEqual #"POST" (mref req 'method))))

(defun new-with-body-test ()
  (let ((req (http.request:new #"POST" "http://example.com" #"test body")))
    (?_assertEqual #"POST" (mref req 'method))
    (?_assertEqual #"test body" (mref req 'body))))

(defun new-with-headers-test ()
  (let* ((headers #m(#"X-Custom" #"value"))
         (req (http.request:new #"POST" "http://example.com" #"body" headers)))
    (?_assertEqual headers (mref req 'headers))))

;;; Method conversion tests
(defun method-conversion-test_ ()
  (list
   ;; Binary (uppercase)
   (?_assertEqual #"GET" (mref (http.request:new #"get" "http://example.com") 'method))
   
   ;; Atom
   (?_assertEqual #"POST" (mref (http.request:new 'post "http://example.com") 'method))
   
   ;; String
   (?_assertEqual #"PUT" (mref (http.request:new "put" "http://example.com") 'method))))

;;; URL parsing tests
(defun url-parsing-test ()
  (let ((req (http.request:new "http://example.com/api/users?id=123&name=test")))
    (?_assert (is_map (mref req 'url-parsed)))
    (?_assert (is_list (mref req 'path-segments)))
    (?_assert (is_map (mref req 'query-parsed)))))

;;; Setter tests
(defun set-method-test ()
  (let* ((req (http.request:new "http://example.com"))
         (req2 (http.request:set-method req #"POST")))
    (?_assertEqual #"POST" (mref req2 'method))
    ;; Original unchanged
    (?_assertEqual #"GET" (mref req 'method))))

(defun set-body-test ()
  (let* ((req (http.request:new "http://example.com"))
         (req2 (http.request:set-body req #"new body")))
    (?_assertEqual #"new body" (mref req2 'body))))

(defun set-header-test ()
  (let* ((req (http.request:new "http://example.com"))
         (req2 (http.request:set-header req #"X-Custom" #"value")))
    (?_assertEqual #"value" 
                   (http.header:get (mref req2 'headers) #"X-Custom"))))

;;; Content-type helper tests
(defun set-json-test ()
  (let* ((req (http.request:new "http://example.com"))
         (json #"{\"key\":\"value\"}")
         (req2 (http.request:set-json req json)))
    (?_assertEqual json (mref req2 'body))
    (?_assertEqual #"application/json; charset=utf-8"
                   (http.header:get-ci (mref req2 'headers) #"Content-Type"))))

(defun set-form-map-test ()
  (let* ((req (http.request:new "http://example.com"))
         (params #m(#"key1" #"value1" #"key2" #"value2"))
         (req2 (http.request:set-form req params))
         (body (mref req2 'body)))
    ;; Body should be URL-encoded
    (?_assert (or (== body #"key1=value1&key2=value2")
                  (== body #"key2=value2&key1=value1")))
    (?_assertEqual #"application/x-www-form-urlencoded"
                   (http.header:get-ci (mref req2 'headers) #"Content-Type"))))

(defun set-text-test ()
  (let* ((req (http.request:new "http://example.com"))
         (text #"Plain text content")
         (req2 (http.request:set-text req text)))
    (?_assertEqual text (mref req2 'body))
    (?_assertEqual #"text/plain; charset=utf-8"
                   (http.header:get-ci (mref req2 'headers) #"Content-Type"))))

;;; Query parameter tests
(defun add-query-param-test ()
  (let* ((req (http.request:new "http://example.com/path"))
         (req2 (http.request:add-query-param req #"key" #"value"))
         (query-map (mref req2 'query-parsed)))
    (?_assertEqual #"value" (maps:get #"key" query-map))
    ;; URL should be updated
    (?_assert (binary:match (mref req2 'url) #"key=value" '()) '/= 'nomatch))))

(defun set-query-params-test ()
  (let* ((req (http.request:new "http://example.com/path?old=param"))
         (new-params #m(#"new" #"param" #"another" #"one"))
         (req2 (http.request:set-query-params req new-params))
         (query-map (mref req2 'query-parsed)))
    (?_assertEqual 2 (maps:size query-map))
    (?_assertEqual #"param" (maps:get #"new" query-map))
    ;; Old param should be gone
    (?_assertEqual 'false (maps:is_key #"old" query-map))))

;;; Threading-style usage test
(defun threading-style-test ()
  (let ((req (-> (http.request:new "http://example.com/api")
                 (http.request:set-method #"POST")
                 (http.request:set-json #"{\"data\":\"value\"}")
                 (http.request:set-header #"Authorization" #"Bearer token"))))
    (?_assertEqual #"POST" (mref req 'method))
    (?_assertEqual #"{\"data\":\"value\"}" (mref req 'body))
    (?_assertEqual #"Bearer token"
                   (http.header:get (mref req 'headers) #"Authorization"))))

;;; Performance test
(defun construction-performance-test ()
  (let ((`#(,_ ,elapsed)
         (http.util:measure
           (lambda ()
             (lists:foreach
               (lambda (_)
                 (http.request:new #"POST" 
                                   "http://example.com/api"
                                   #"body"
                                   #m(#"X-Custom" #"value")))
               (lists:seq 1 1000))))))
    (io:format "1000 request constructions in ~pμs (~.2fμs/op)~n"
               (list elapsed (/ elapsed 1000.0)))
    (?_assert (< elapsed 100000)))) ; Should be under 100ms for 1000 ops
```

---

## Benchmarking Requirements

### Benchmark File: `bench/request-bench.lfe`

```lfe
(defmodule request-bench
  (export
   (run 0)
   (bench-construction 0)
   (bench-setters 0)
   (bench-helpers 0)))

(defun run ()
  (io:format "~n=== HTTP Request Benchmarks ===~n")
  (bench-construction)
  (bench-setters)
  (bench-helpers))

(defun bench-construction ()
  (io:format "~nRequest construction performance:~n")
  
  ;; Simple URL only
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) 
                  (http.request:new "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format "  new/1 (URL only): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))
  
  ;; With method
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format "  new/2 (method + URL): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))
  
  ;; Full construction
  (let* ((iterations 10000)
         (headers #m(#"X-Custom" #"value"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com" #"body" headers))
                (lists:seq 1 iterations))))))
    (io:format "  new/4 (full): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun bench-setters ()
  (io:format "~nSetter operations performance:~n")
  
  (let* ((req (http.request:new "http://example.com"))
         (iterations 100000))
    
    ;; set-method
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-method req #"POST"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-method: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))
    
    ;; set-body
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-body req #"body"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-body: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))
    
    ;; set-header
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-header req #"X-Custom" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-header: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-helpers ()
  (io:format "~nHelper functions performance:~n")
  
  (let* ((req (http.request:new "http://example.com"))
         (json #"{\"key\":\"value\"}")
         (iterations 10000))
    
    ;; set-json
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-json req json))
                 (lists:seq 1 iterations))))))
      (io:format "  set-json: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))
    
    ;; add-query-param
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:add-query-param req #"key" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format "  add-query-param: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))
```

---

## Performance Expectations

### Targets
- **new/1**: 8-15μs per operation (single URL parse)
- **new/4**: 10-20μs per operation (40-60% fewer allocations vs v0.5.4)
- **Setters**: 50-150ns per operation (map update only)
- **Helpers**: 1-3μs per operation (includes header updates)

---

## Completion Checklist

- [ ] `http.request.lfe` fully rewritten
- [ ] All constructors use direct map construction
- [ ] Builder pattern functions implemented
- [ ] Content-type helpers working
- [ ] Query parameter helpers working
- [ ] Threading-style operations work
- [ ] All tests pass
- [ ] Benchmarks show 40%+ allocation reduction
- [ ] No compiler warnings

---

## Success Criteria

1. ✅ All tests pass
2. ✅ Benchmarks show ≥40% allocation reduction
3. ✅ Single URL parse confirmed
4. ✅ Threading-style operations work smoothly
5. ✅ Code follows LFE style guidelines

---

## Next Steps

After Phase 3, proceed to **Phase 4: Response Builder**