# Phase 1: Core Infrastructure & Utilities

## Overview
Build the foundation modules that all other phases depend on. This phase creates the binary conversion utilities, HTTP method macros, MIME type constants, and core library API.

**Estimated Time**: 2-3 hours  
**Dependencies**: None (first phase)  
**Modules**: `http.util.lfe`, `http.lfe`, `http.mimetype.lfe` (NEW)

---

## Objectives

1. ✅ Create optimized binary conversion utilities
2. ✅ Define HTTP method macros for compile-time optimization
3. ✅ Build MIME type system (missing in v0.5.4!)
4. ✅ Add performance measurement utilities
5. ✅ Set up inline directives for hot-path functions
6. ✅ Maintain backward-compatible library metadata API

---

## Module 1: http.util.lfe

### Current State (v0.5.4)
```lfe
(defmodule http.util
  (export
   (add-header 3)
   (http-version 1)))

(defun add-header (req-or-resp k v)
  (let* ((hs (mref req-or-resp 'headers))
         (hs (http.header:add hs k v)))
    (maps:put 'headers hs req-or-resp)))

(defun http-version (req)
  (io_lib:format "HTTP/~p" (list (mref req 'version))))
```

### New Implementation

**File**: `src/http.util.lfe`

```lfe
(defmodule http.util
  (export
   ;; Legacy API (keep for compatibility)
   (add-header 3)
   (http-version 1)
   
   ;; New binary utilities
   (ensure-binary 1)
   (binary-upcase 1)
   (binary-downcase 1)
   (binary-downcase-atom 1)
   
   ;; HTTP version utilities
   (http-version-tuple 1)
   (http-version-string 1)
   
   ;; URL utilities
   (join-path 1)
   (query-string 1)
   
   ;; Performance utilities
   (measure 1)
   (measure 2)))

;; Compiler directives - inline hot-path functions
(compile (inline ensure-binary 1))
(compile (inline binary-upcase 1))
(compile (inline binary-downcase 1))

;;; ---------------------------------------------------------------------------
;;; Binary Conversion Utilities
;;; ---------------------------------------------------------------------------

(defun ensure-binary
  "Convert any value to binary. Optimized for hot-path usage.
  
  Args:
    val: Binary, list, atom, or integer to convert
    
  Returns:
    Binary representation of val"
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (iolist_to_binary l))
  ((a) (when (is_atom a)) (atom_to_binary a))
  ((i) (when (is_integer i)) (integer_to_binary i)))

(defun binary-upcase
  "Convert binary to uppercase.
  
  Args:
    bin: Binary string to uppercase
    
  Returns:
    Uppercased binary"
  ((bin) (when (is_binary bin))
   (list_to_binary (string:uppercase (binary_to_list bin)))))

(defun binary-downcase
  "Convert binary to lowercase.
  
  Args:
    bin: Binary string to lowercase
    
  Returns:
    Lowercased binary"
  ((bin) (when (is_binary bin))
   (list_to_binary (string:lowercase (binary_to_list bin)))))

(defun binary-downcase-atom
  "Convert binary to lowercase atom.
  
  Args:
    bin: Binary string to convert
    
  Returns:
    Lowercase atom"
  ((bin) (when (is_binary bin))
   (binary_to_atom (binary-downcase bin))))

;;; ---------------------------------------------------------------------------
;;; HTTP Version Utilities
;;; ---------------------------------------------------------------------------

(defun http-version
  "Get HTTP version string from request/response map.
  Legacy API - maintained for compatibility.
  
  Args:
    req-or-resp: Request or response map with 'version key
    
  Returns:
    String like \"HTTP/1.1\""
  ((req-or-resp)
   (http-version-string (mref req-or-resp 'version))))

(defun http-version-string
  "Convert version number to HTTP version string.
  
  Args:
    version: Float (1.0, 1.1, 2, 3) or tuple
    
  Returns:
    Binary like #\"HTTP/1.1\""
  ((1.0) #"HTTP/1.0")
  ((1.1) #"HTTP/1.1")
  ((2) #"HTTP/2")
  ((2.0) #"HTTP/2")
  ((3) #"HTTP/3")
  ((3.0) #"HTTP/3")
  ((`#(,major ,minor))
   (list_to_binary (io_lib:format "HTTP/~p.~p" (list major minor))))
  ((version)
   (list_to_binary (io_lib:format "HTTP/~p" (list version)))))

(defun http-version-tuple
  "Convert version number to tuple for Erlang httpc.
  
  Args:
    version: Float (1.0, 1.1, 2, 3) or tuple
    
  Returns:
    Tuple like #(1 1) for httpc"
  ((1.0) #(1 0))
  ((1.1) #(1 1))
  ((2) #(2 0))
  ((2.0) #(2 0))
  ((3) #(3 0))
  ((3.0) #(3 0))
  ((`#(,major ,minor)) `#(,major ,minor))
  ((version) `#(,version 0)))

;;; ---------------------------------------------------------------------------
;;; Header Utilities
;;; ---------------------------------------------------------------------------

(defun add-header
  "Add a header to request or response map.
  Legacy API - maintained for compatibility.
  
  Args:
    req-or-resp: Request or response map
    key: Header name (will be converted to binary)
    val: Header value (will be converted to binary)
    
  Returns:
    Updated request or response map"
  ((req-or-resp key val)
   (let* ((headers (mref req-or-resp 'headers))
          (key-bin (ensure-binary key))
          (val-bin (ensure-binary val))
          (updated-headers (maps:put key-bin val-bin headers)))
     (mset req-or-resp 'headers updated-headers))))

;;; ---------------------------------------------------------------------------
;;; URL Utilities
;;; ---------------------------------------------------------------------------

(defun join-path
  "Join path segments into a URL path.
  
  Args:
    segments: List of binary path segments
    
  Returns:
    Binary path like #\"/api/v1/users\""
  ((segments) (when (is_list segments))
   (iolist_to_binary 
     (lists:join #"/" segments))))

(defun query-string
  "Convert map of query parameters to query string.
  
  Args:
    params: Map of binary keys to binary values
    
  Returns:
    Binary query string like #\"key1=val1&key2=val2\""
  ((params) (when (is_map params))
   (let ((pairs (maps:fold
                  (lambda (k v acc)
                    (let ((key (uri-encode (ensure-binary k)))
                          (val (uri-encode (ensure-binary v))))
                      (cons (iolist_to_binary (list key #"=" val)) acc)))
                  '()
                  params)))
     (case pairs
       ('() #"")
       (_ (iolist_to_binary (lists:join #"&" (lists:reverse pairs))))))))

(defun uri-encode
  "URL-encode a binary string (basic implementation).
  
  Args:
    bin: Binary to encode
    
  Returns:
    URL-encoded binary"
  ((bin) (when (is_binary bin))
   ;; Simple implementation - encode common special chars
   ;; For production, consider using uri_string:quote/1 (OTP 23+)
   (list_to_binary 
     (lists:flatten
       (lists:map
         (lambda (c)
           (if (or (andalso (>= c 48) (<= c 57))   ; 0-9
                   (andalso (>= c 65) (<= c 90))   ; A-Z
                   (andalso (>= c 97) (<= c 122))  ; a-z
                   (== c 45)                        ; -
                   (== c 95)                        ; _
                   (== c 46)                        ; .
                   (== c 126))                      ; ~
             (list c)
             (io_lib:format "%~2.16.0B" (list c))))
         (binary_to_list bin))))))

;;; ---------------------------------------------------------------------------
;;; Performance Utilities
;;; ---------------------------------------------------------------------------

(defun measure
  "Measure execution time of a function in microseconds.
  
  Args:
    fun: Zero-arity function to measure
    
  Returns:
    Tuple #(result elapsed-microseconds)"
  ((fun) (when (is_function fun 0))
   (let ((start (erlang:monotonic_time 'microsecond)))
     (let ((result (funcall fun)))
       (let ((elapsed (- (erlang:monotonic_time 'microsecond) start)))
         `#(,result ,elapsed))))))

(defun measure
  "Measure execution time with custom time unit.
  
  Args:
    fun: Zero-arity function to measure
    unit: Time unit (microsecond, millisecond, second)
    
  Returns:
    Tuple #(result elapsed-time)"
  ((fun unit) (when (andalso (is_function fun 0) (is_atom unit)))
   (let ((start (erlang:monotonic_time unit)))
     (let ((result (funcall fun)))
       (let ((elapsed (- (erlang:monotonic_time unit) start)))
         `#(,result ,elapsed))))))
```

### Implementation Notes

1. **Inline directives**: Mark `ensure-binary`, `binary-upcase`, `binary-downcase` for inlining
2. **Pattern matching**: Use guards extensively for type safety
3. **Zero-copy**: `ensure-binary` returns input if already binary
4. **URL encoding**: Basic implementation provided, can be enhanced later
5. **Measurement**: Provides microsecond precision for benchmarking

---

## Module 2: http.lfe

### Current State (v0.5.4)
```lfe
(defmodule http
  (export
   (default-headers 0)
   (default-version 0)
   (methods 0)
   (versions 0)))

(defun default-headers ()
  (http.header:new))

(defun default-version ()
  1.1)

(defun methods ()
  '(delete get head options patch post put trace))

(defun versions ()
  '(0.9 1.0 1.1 2 3))
```

### New Implementation

**File**: `src/http.lfe`

```lfe
(defmodule http
  (export
   ;; Legacy API (keep for compatibility)
   (default-headers 0)
   (default-version 0)
   (methods 0)
   (versions 0)
   
   ;; New binary method utilities
   (valid-method? 1)
   (normalize-method 1)
   (method-has-body? 1)))

;;; ---------------------------------------------------------------------------
;;; HTTP Method Constants (Macros for compile-time optimization)
;;; ---------------------------------------------------------------------------

;; These macros generate binary constants at compile time
(defmacro method-get () #"GET")
(defmacro method-post () #"POST")
(defmacro method-put () #"PUT")
(defmacro method-delete () #"DELETE")
(defmacro method-patch () #"PATCH")
(defmacro method-head () #"HEAD")
(defmacro method-options () #"OPTIONS")
(defmacro method-trace () #"TRACE")
(defmacro method-connect () #"CONNECT")

;;; ---------------------------------------------------------------------------
;;; Legacy API (Maintained for Compatibility)
;;; ---------------------------------------------------------------------------

(defun default-headers
  "Return default headers map for new requests.
  
  Returns:
    Map with User-Agent and Accept headers"
  ()
  #m(#"User-Agent" #"lfe-http/1.0.0"
     #"Accept" #"*/*"))

(defun default-version
  "Return default HTTP version.
  
  Returns:
    Float 1.1"
  ()
  1.1)

(defun methods
  "Return list of supported HTTP methods as atoms (legacy).
  
  Returns:
    List of method atoms"
  ()
  '(delete get head options patch post put trace connect))

(defun versions
  "Return list of supported HTTP versions.
  
  Returns:
    List of version numbers"
  ()
  '(0.9 1.0 1.1 2 3))

;;; ---------------------------------------------------------------------------
;;; New Binary Method API
;;; ---------------------------------------------------------------------------

(defun valid-method?
  "Check if a method is a valid HTTP method.
  
  Args:
    method: Binary method name (e.g., #\"GET\")
    
  Returns:
    Boolean true/false"
  ((method) (when (is_binary method))
   (case method
     (#"GET" 'true)
     (#"POST" 'true)
     (#"PUT" 'true)
     (#"DELETE" 'true)
     (#"PATCH" 'true)
     (#"HEAD" 'true)
     (#"OPTIONS" 'true)
     (#"TRACE" 'true)
     (#"CONNECT" 'true)
     (_ 'false)))
  ((_) 'false))

(defun normalize-method
  "Convert method from any format to uppercase binary.
  
  Args:
    method: Atom, string, or binary method
    
  Returns:
    Uppercase binary method (e.g., #\"GET\")"
  ((method) (when (is_binary method))
   (http.util:binary-upcase method))
  ((method) (when (is_atom method))
   (http.util:binary-upcase (atom_to_binary method)))
  ((method) (when (is_list method))
   (http.util:binary-upcase (list_to_binary method))))

(defun method-has-body?
  "Check if an HTTP method typically has a request body.
  
  Args:
    method: Binary method name
    
  Returns:
    Boolean true/false"
  ((#"POST") 'true)
  ((#"PUT") 'true)
  ((#"PATCH") 'true)
  ((#"DELETE") 'false)  ; Usually no body, but can have one
  ((_) 'false))
```

### Implementation Notes

1. **Macros for methods**: Compile-time constants, zero runtime cost
2. **Backward compatibility**: Keep `methods()` and `versions()` for existing code
3. **Binary validation**: Fast binary pattern matching in `valid-method?`
4. **Default headers**: Pre-built map, no construction cost
5. **Method normalization**: Handle any input type, convert to binary

---

## Module 3: http.mimetype.lfe (NEW)

### Current State
**Does not exist!** Referenced in `http.c.lfe` but missing.

### New Implementation

**File**: `src/http.mimetype.lfe`

```lfe
(defmodule http.mimetype
  (export
   ;; Common MIME types
   (from-extension 1)
   (from-path 1)
   
   ;; Binary constants (for backward compatibility if needed)
   (text/plain 0)
   (text/html 0)
   (application/json 0)
   (application/xml 0)
   (application/octet-stream 0)
   (multipart/form-data 0)))

;;; ---------------------------------------------------------------------------
;;; MIME Type Constants (Macros for compile-time optimization)
;;; ---------------------------------------------------------------------------

(defmacro TEXT-PLAIN () #"text/plain; charset=utf-8")
(defmacro TEXT-HTML () #"text/html; charset=utf-8")
(defmacro TEXT-CSS () #"text/css; charset=utf-8")
(defmacro TEXT-JAVASCRIPT () #"text/javascript; charset=utf-8")
(defmacro APPLICATION-JSON () #"application/json; charset=utf-8")
(defmacro APPLICATION-XML () #"application/xml; charset=utf-8")
(defmacro APPLICATION-OCTET-STREAM () #"application/octet-stream")
(defmacro MULTIPART-FORM-DATA () #"multipart/form-data")
(defmacro IMAGE-PNG () #"image/png")
(defmacro IMAGE-JPEG () #"image/jpeg")
(defmacro IMAGE-GIF () #"image/gif")
(defmacro IMAGE-SVG () #"image/svg+xml")

;;; ---------------------------------------------------------------------------
;;; Function API (for dynamic lookups)
;;; ---------------------------------------------------------------------------

(defun text/plain () #"text/plain; charset=utf-8")
(defun text/html () #"text/html; charset=utf-8")
(defun text/css () #"text/css; charset=utf-8")
(defun application/json () #"application/json; charset=utf-8")
(defun application/xml () #"application/xml; charset=utf-8")
(defun application/octet-stream () #"application/octet-stream")
(defun multipart/form-data () #"multipart/form-data")

;;; ---------------------------------------------------------------------------
;;; Extension to MIME Type Mapping
;;; ---------------------------------------------------------------------------

(defun mime-map
  "Return a pre-built map of file extensions to MIME types.
  This map is created once per call, consider caching if used frequently.
  
  Returns:
    Map of binary extensions to binary MIME types"
  ()
  #m(;; Text formats
     #"txt"  #"text/plain; charset=utf-8"
     #"html" #"text/html; charset=utf-8"
     #"htm"  #"text/html; charset=utf-8"
     #"css"  #"text/css; charset=utf-8"
     #"csv"  #"text/csv; charset=utf-8"
     
     ;; Application formats
     #"json" #"application/json; charset=utf-8"
     #"xml"  #"application/xml; charset=utf-8"
     #"js"   #"application/javascript; charset=utf-8"
     #"pdf"  #"application/pdf"
     #"zip"  #"application/zip"
     #"gz"   #"application/gzip"
     #"tar"  #"application/x-tar"
     
     ;; Image formats
     #"png"  #"image/png"
     #"jpg"  #"image/jpeg"
     #"jpeg" #"image/jpeg"
     #"gif"  #"image/gif"
     #"svg"  #"image/svg+xml"
     #"webp" #"image/webp"
     #"ico"  #"image/x-icon"
     
     ;; Audio formats
     #"mp3"  #"audio/mpeg"
     #"wav"  #"audio/wav"
     #"ogg"  #"audio/ogg"
     
     ;; Video formats
     #"mp4"  #"video/mp4"
     #"webm" #"video/webm"
     #"avi"  #"video/x-msvideo"
     
     ;; Font formats
     #"woff" #"font/woff"
     #"woff2" #"font/woff2"
     #"ttf"  #"font/ttf"
     #"otf"  #"font/otf"))

(defun from-extension
  "Get MIME type from file extension.
  
  Args:
    ext: Binary file extension (without dot), e.g., #\"json\"
    
  Returns:
    Binary MIME type, or application/octet-stream if unknown"
  ((ext) (when (is_binary ext))
   (let ((ext-lower (http.util:binary-downcase ext)))
     (maps:get ext-lower (mime-map) #"application/octet-stream")))
  ((ext) (when (is_list ext))
   (from-extension (list_to_binary ext)))
  ((ext) (when (is_atom ext))
   (from-extension (atom_to_binary ext))))

(defun from-path
  "Get MIME type from file path by extracting extension.
  
  Args:
    path: Binary file path, e.g., #\"/path/to/file.json\"
    
  Returns:
    Binary MIME type, or application/octet-stream if unknown"
  ((path) (when (is_binary path))
   (case (binary:split path #"." '(global))
     ('() #"application/octet-stream")
     (parts
      (let ((ext (lists:last parts)))
        (from-extension ext)))))
  ((path) (when (is_list path))
   (from-path (list_to_binary path))))
```

### Implementation Notes

1. **Macros + Functions**: Macros for compile-time, functions for dynamic use
2. **Comprehensive map**: Common file types covered
3. **Case insensitive**: Extension matching is case-insensitive
4. **Charset included**: Text types include UTF-8 charset
5. **Fallback**: Returns `application/octet-stream` for unknown types

---

## Testing Requirements

### Test File: `test/http-util-tests.lfe`

```lfe
(defmodule http-util-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

;;; Binary conversion tests
(defun ensure-binary-test_ ()
  (list
   ;; Binary input (no-op)
   (?_assertEqual #"test" (http.util:ensure-binary #"test"))
   
   ;; List input
   (?_assertEqual #"test" (http.util:ensure-binary "test"))
   
   ;; Atom input
   (?_assertEqual #"test" (http.util:ensure-binary 'test))
   
   ;; Integer input
   (?_assertEqual #"123" (http.util:ensure-binary 123))
   
   ;; iolist input
   (?_assertEqual #"hello world" 
                  (http.util:ensure-binary '(#"hello" " " "world")))))

(defun binary-upcase-test_ ()
  (list
   (?_assertEqual #"GET" (http.util:binary-upcase #"get"))
   (?_assertEqual #"POST" (http.util:binary-upcase #"post"))
   (?_assertEqual #"ALREADY" (http.util:binary-upcase #"ALREADY"))))

(defun http-version-string-test_ ()
  (list
   (?_assertEqual #"HTTP/1.0" (http.util:http-version-string 1.0))
   (?_assertEqual #"HTTP/1.1" (http.util:http-version-string 1.1))
   (?_assertEqual #"HTTP/2" (http.util:http-version-string 2))
   (?_assertEqual #"HTTP/3" (http.util:http-version-string 3))))

(defun query-string-test_ ()
  (list
   ;; Empty map
   (?_assertEqual #"" (http.util:query-string #m()))
   
   ;; Single param
   (?_assertEqual #"key=value" 
                  (http.util:query-string #m(#"key" #"value")))
   
   ;; Multiple params (order not guaranteed, need to parse)
   (let ((qs (http.util:query-string #m(#"a" #"1" #"b" #"2"))))
     (?_assert (or (== qs #"a=1&b=2")
                   (== qs #"b=2&a=1"))))))

(defun measure-test ()
  (let ((`#(,result ,elapsed) 
         (http.util:measure 
           (lambda () 
             (timer:sleep 10)
             'ok))))
    (?_assertEqual 'ok result)
    ;; Should take at least 10ms (10000 microseconds)
    (?_assert (>= elapsed 10000))))
```

### Test File: `test/http-tests.lfe`

```lfe
(defmodule http-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun default-headers-test ()
  (let ((headers (http:default-headers)))
    (?_assertEqual 'true (is_map headers))
    (?_assert (maps:is_key #"User-Agent" headers))
    (?_assert (maps:is_key #"Accept" headers))))

(defun valid-method?-test_ ()
  (list
   ;; Valid methods
   (?_assertEqual 'true (http:valid-method? #"GET"))
   (?_assertEqual 'true (http:valid-method? #"POST"))
   (?_assertEqual 'true (http:valid-method? #"PUT"))
   (?_assertEqual 'true (http:valid-method? #"DELETE"))
   
   ;; Invalid methods
   (?_assertEqual 'false (http:valid-method? #"INVALID"))
   (?_assertEqual 'false (http:valid-method? "GET"))
   (?_assertEqual 'false (http:valid-method? 'get))))

(defun normalize-method-test_ ()
  (list
   ;; Binary input
   (?_assertEqual #"GET" (http:normalize-method #"get"))
   (?_assertEqual #"POST" (http:normalize-method #"Post"))
   
   ;; Atom input
   (?_assertEqual #"GET" (http:normalize-method 'get))
   (?_assertEqual #"POST" (http:normalize-method 'POST))
   
   ;; String input
   (?_assertEqual #"GET" (http:normalize-method "get"))
   (?_assertEqual #"POST" (http:normalize-method "post"))))

(defun method-has-body?-test_ ()
  (list
   ;; Methods with body
   (?_assertEqual 'true (http:method-has-body? #"POST"))
   (?_assertEqual 'true (http:method-has-body? #"PUT"))
   (?_assertEqual 'true (http:method-has-body? #"PATCH"))
   
   ;; Methods without body
   (?_assertEqual 'false (http:method-has-body? #"GET"))
   (?_assertEqual 'false (http:method-has-body? #"HEAD"))
   (?_assertEqual 'false (http:method-has-body? #"DELETE"))))
```

### Test File: `test/http-mimetype-tests.lfe`

```lfe
(defmodule http-mimetype-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun from-extension-test_ ()
  (list
   ;; Text formats
   (?_assertEqual #"text/html; charset=utf-8" 
                  (http.mimetype:from-extension #"html"))
   (?_assertEqual #"text/html; charset=utf-8"
                  (http.mimetype:from-extension #"htm"))
   (?_assertEqual #"text/plain; charset=utf-8"
                  (http.mimetype:from-extension #"txt"))
   
   ;; Application formats
   (?_assertEqual #"application/json; charset=utf-8"
                  (http.mimetype:from-extension #"json"))
   (?_assertEqual #"application/xml; charset=utf-8"
                  (http.mimetype:from-extension #"xml"))
   
   ;; Image formats
   (?_assertEqual #"image/png" 
                  (http.mimetype:from-extension #"png"))
   (?_assertEqual #"image/jpeg"
                  (http.mimetype:from-extension #"jpg"))
   
   ;; Unknown extension
   (?_assertEqual #"application/octet-stream"
                  (http.mimetype:from-extension #"unknown"))
   
   ;; Case insensitive
   (?_assertEqual #"text/html; charset=utf-8"
                  (http.mimetype:from-extension #"HTML"))))

(defun from-path-test_ ()
  (list
   (?_assertEqual #"text/html; charset=utf-8"
                  (http.mimetype:from-path #"/path/to/file.html"))
   (?_assertEqual #"application/json; charset=utf-8"
                  (http.mimetype:from-path #"/api/data.json"))
   (?_assertEqual #"image/png"
                  (http.mimetype:from-path #"/images/logo.png"))
   (?_assertEqual #"application/octet-stream"
                  (http.mimetype:from-path #"/no/extension"))))
```

---

## Benchmarking Requirements

### Benchmark File: `bench/util-bench.lfe`

```lfe
(defmodule util-bench
  (export 
   (run 0)
   (bench-ensure-binary 0)
   (bench-binary-upcase 0)
   (bench-query-string 0)))

(defun run ()
  (io:format "~n=== HTTP Util Benchmarks ===~n")
  (bench-ensure-binary)
  (bench-binary-upcase)
  (bench-query-string))

(defun bench-ensure-binary ()
  (io:format "~nensure-binary/1 performance:~n")
  
  ;; Binary input (should be instant)
  (let* ((iterations 1000000)
         (`#(,_ ,elapsed) 
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:ensure-binary #"test"))
                (lists:seq 1 iterations))))))
    (io:format "  Binary input: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations))))
  
  ;; Atom input (conversion required)
  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:ensure-binary 'test))
                (lists:seq 1 iterations))))))
    (io:format "  Atom input: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

(defun bench-binary-upcase ()
  (io:format "~nbinary-upcase/1 performance:~n")
  
  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:binary-upcase #"content-type"))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

(defun bench-query-string ()
  (io:format "~nquery-string/1 performance:~n")
  
  (let* ((params #m(#"key1" #"value1"
                    #"key2" #"value2"
                    #"key3" #"value3"))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:query-string params))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
```

---

## Completion Checklist

- [ ] `http.util.lfe` implemented with all functions
- [ ] `http.lfe` implemented with macros and utilities
- [ ] `http.mimetype.lfe` created from scratch
- [ ] Inline directives added to hot-path functions
- [ ] All unit tests pass
- [ ] Benchmarks run successfully
- [ ] No compiler warnings
- [ ] Documentation comments added to all exported functions

---

## Success Criteria

1. ✅ All three modules compile without warnings
2. ✅ All tests pass (run with `rebar3 eunit`)
3. ✅ `ensure-binary` handles all input types correctly
4. ✅ Method macros generate binary constants
5. ✅ MIME type lookups work for common extensions
6. ✅ Performance benchmarks show expected results
7. ✅ Code follows LFE style guidelines

---

## Next Steps

After completing Phase 1:
1. Commit changes with message: "Phase 1: Core infrastructure and utilities"
2. Tag commit: `v1.0.0-phase1`
3. Proceed to **Phase 2: Header Management System**
4. Phase 2 will use utilities from this phase extensively