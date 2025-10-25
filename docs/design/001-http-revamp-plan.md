# LFE HTTP Library v1.0.0 - Complete Revamp Plan

## Executive Summary
Complete rewrite focusing on performance, binary-first operations, and idiomatic LFE. This is a **breaking release** (v0.5.4 → v1.0.0).

---

## Core Principles

1. **Binary-First**: Use binaries throughout, eliminate unnecessary conversions
2. **Zero-Copy Where Possible**: Pass references, not copies
3. **Direct Matching**: Use binary patterns instead of atom conversions
4. **Idiomatic LFE**: Leverage pattern matching, guards, and functional composition
5. **Inline Hot Paths**: Mark critical functions for inlining
6. **Reduce Allocations**: Pre-allocate, reuse structures

---

## Module-by-Module Changes

### 1. `http.lfe` (Core Module)
**Current Issues:**
- Returns method atoms as list
- Generic default headers function

**Changes:**
```lfe
; OLD: '(delete get head options patch post put trace)
; NEW: Binary method constants, compile-time macros

(defmacro method-get () #"GET")
(defmacro method-post () #"POST")
(defmacro method-put () #"PUT")
(defmacro method-delete () #"DELETE")
(defmacro method-patch () #"PATCH")
(defmacro method-head () #"HEAD")
(defmacro method-options () #"OPTIONS")
(defmacro method-trace () #"TRACE")

; Fast binary method validation (inline)
(defun valid-method? (method) 
  ; Use binary pattern matching in guard
  )

; Default headers as pre-built binary map
(defun default-headers () 
  #m(#"User-Agent" #"lfe-http/1.0.0"
     #"Accept" #"*/*"))
```

**Benefits:**
- Compile-time constants (no runtime allocation)
- Direct binary matching
- Inline-able predicates

---

### 2. `http.header.lfe` (Header Management)
**Current Issues:**
- `kv->bins` does recursive conversions (slow)
- Multiple passes: atom→list→binary
- `list->map` creates intermediate list

**Changes:**
```lfe
; NEW: Direct binary operations, single-pass
(defun add (headers key val)
  ; Accept binary key/val directly, inline
  ; Pattern match on types, convert in-place
  )

(defun new () 
  ; Return empty map typed for binaries
  #m())

(defun from-list (proplist)
  ; Single-pass conversion with fold
  ; No intermediate list allocation
  (lists:foldl #'add-kv/2 #m() proplist))

(defun add-kv (acc kv)
  ; Pattern match directly on tuple types
  ; Convert inline without recursion
  )

; NEW: Case-insensitive header lookup (HTTP spec)
(defun get-ci (headers key)
  ; Binary case-insensitive match
  ; Use lowercase binary keys internally
  )

; NEW: Bulk operations
(defun merge (h1 h2)
  ; Fast map merge
  (maps:merge h1 h2))
```

**Benefits:**
- 50-70% faster header processing
- Single allocation per header
- Case-insensitive lookups (HTTP standard)

---

### 3. `http.request.lfe` (Request Building)
**Current Issues:**
- Multiple `maps:merge` calls
- Parses URL multiple times
- Unnecessary intermediate structures
- `->list` for debugging only (slow)

**Changes:**
```lfe
; NEW: Efficient builder pattern
(defun new (url)
  ; Single parse, binary URL
  (let* ((parsed (yuri:parse (ensure-binary url)))
         (path-segs (yuri.path:->segments (mref parsed 'path)))
         (query-map (yuri.query:parse (mref parsed 'query))))
    #m(method #"GET"
       version 1.1
       headers (http:default-headers)
       body #""
       url url
       url-parsed parsed
       path-segments path-segs
       query-parsed query-map)))

; NEW: Efficient multi-arity constructors
(defun new (method url)
  (mset (new url) 'method (ensure-method-binary method)))

(defun new (method url body)
  (-> (new url)
      (mset 'method (ensure-method-binary method))
      (mset 'body (ensure-binary body))))

(defun new (method url body headers)
  (-> (new url)
      (mset 'method (ensure-method-binary method))
      (mset 'body (ensure-binary body))
      (mset 'headers (ensure-header-map headers))))

; NEW: Fast method normalization (inline)
(defun ensure-method-binary
  ((m) (when (is_binary m)) (binary-upcase m))
  ((m) (when (is_atom m)) 
   (binary-upcase (atom_to_binary m)))
  ((m) (when (is_list m))
   (binary-upcase (list_to_binary m))))

; REMOVE: ->list (move to debug module if needed)

; NEW: Efficient header addition
(defun add-header (req key val)
  ; Direct map update, no utility function
  (let ((hs (mref req 'headers)))
    (mset req 'headers (http.header:add hs key val))))

; NEW: Fast content-type helpers
(defun set-json (req body)
  (-> req
      (mset 'body body)
      (add-header #"Content-Type" #"application/json")))
```

**Benefits:**
- Single URL parse
- Reduced allocations (50-60% fewer)
- Threading macro for readability
- Fast path for binary inputs

---

### 4. `http.response.lfe` (Response Building)
**Current Issues:**
- Multiple function calls for simple operations
- `set-body` does excessive type checking
- No streaming support

**Changes:**
```lfe
; NEW: Efficient constructors
(defun new ()
  #m(status 200
     headers #m()
     body #""
     version 1.1))

(defun new (status)
  (mset (new) 'status status))

(defun new (status body)
  (-> (new)
      (mset 'status status)
      (mset 'body (ensure-binary body))))

(defun new (status headers body)
  #m(status status
     headers headers
     body (ensure-binary body)
     version 1.1))

; NEW: Fast body setting (inline candidate)
(defun set-body (resp body)
  (mset resp 'body (ensure-binary body)))

(defun ensure-binary
  ((b) (when (is_binary b)) b)
  ((b) (when (is_list b)) (iolist_to_binary b))
  ((b) (when (is_atom b)) (atom_to_binary b))
  ((b) (when (is_integer b)) (integer_to_binary b)))

; NEW: Fast header operations
(defun add-header (resp key val)
  (let ((hs (mref resp 'headers)))
    (mset resp 'headers (maps:put key val hs))))

; NEW: Common response builders (inline)
(defun ok (body) (new 200 body))
(defun created (body) (new 201 body))
(defun no-content () (new 204 #""))
(defun bad-request (body) (new 400 body))
(defun not-found (body) (new 404 body))
(defun error (body) (new 500 body))

; NEW: JSON response helper
(defun json (status data)
  (-> (new status (encode-json data))
      (add-header #"Content-Type" #"application/json; charset=utf-8")))

; NEW: Streaming support (future)
(defun new-stream (status headers)
  #m(status status
     headers headers
     body streaming
     stream-ref (make_ref)))
```

**Benefits:**
- Direct map construction (fastest)
- Common cases optimized
- Inline-able helpers
- Foundation for streaming

---

### 5. `http.c.lfe` (Erlang httpc Interop)
**Current Issues:**
- Most complex module with heavy conversions
- Method matching via atoms (slow)
- Multiple conversion passes
- Creates intermediate lists

**Changes:**
```lfe
; NEW: Binary method dispatch (compile-time optimization)
(defun ->erlang (req http-options options)
  (->erlang-dispatch (mref req 'method) req http-options options))

; NEW: Pattern match on binary methods directly
(defun ->erlang-dispatch
  ; No-body methods
  ((#"GET" req opts1 opts2) (->erlang-no-body 'get req opts1 opts2))
  ((#"HEAD" req opts1 opts2) (->erlang-no-body 'head req opts1 opts2))
  ((#"DELETE" req opts1 opts2) (->erlang-no-body 'delete req opts1 opts2))
  ((#"OPTIONS" req opts1 opts2) (->erlang-no-body 'options req opts1 opts2))
  ((#"TRACE" req opts1 opts2) (->erlang-no-body 'trace req opts1 opts2))
  ; Body methods
  ((#"POST" req opts1 opts2) (->erlang-with-body 'post req opts1 opts2))
  ((#"PUT" req opts1 opts2) (->erlang-with-body 'put req opts1 opts2))
  ((#"PATCH" req opts1 opts2) (->erlang-with-body 'patch req opts1 opts2))
  ; Fallback for custom methods
  ((method req opts1 opts2)
   (->erlang-with-body (binary-downcase-atom method) req opts1 opts2)))

; NEW: Optimized no-body conversion
(defun ->erlang-no-body (method-atom req http-opts opts)
  (let* ((url (mref req 'url))
         (headers (mref req 'headers)))
    ; Direct tuple construction, no intermediate list
    `(,method-atom
      #(,url ,(maps:to_list headers))
      ,http-opts
      ,opts)))

; NEW: Optimized with-body conversion
(defun ->erlang-with-body (method-atom req http-opts opts)
  (let* ((url (mref req 'url))
         (headers (mref req 'headers))
         (body (mref req 'body))
         (content-type (maps:get #"Content-Type" headers 
                                  #"application/octet-stream")))
    `(,method-atom
      #(,url 
        ,(maps:to_list headers)
        ,(binary_to_list content-type)
        ,body)
      ,http-opts
      ,(cons #(body_format binary) opts))))

; NEW: Faster response conversion (single-pass)
(defun erlang-> (httpc-resp)
  (let ((`#(#(,_version ,status ,_reason) ,header-list ,body) httpc-resp))
    ; Single pass header conversion
    #m(status status
       headers (http.header:from-list header-list)
       body (iolist_to_binary body)
       version 1.1)))

; NEW: Request wrapper with smart defaults
(defun request (url)
  (request-internal (http.request:new url)))

(defun request (method url)
  (request-internal (http.request:new method url)))

(defun request (method url body)
  (request-internal (http.request:new method url body)))

(defun request (method url body headers)
  (request-internal (http.request:new method url body headers)))

; NEW: Single internal request function
(defun request-internal (req)
  (request-internal req '() '()))

(defun request-internal (req http-opts)
  (request-internal req http-opts '()))

(defun request-internal (req http-opts opts)
  (let* ((version (mref req 'version))
         (http-opts2 (cons `#(version ,(http.util:http-version-tuple version))
                           http-opts))
         (opts2 (lists:append opts '(#(sync true) #(full_result true))))
         (args (->erlang req http-opts2 opts2)))
    (case (apply #'httpc:request/4 args)
      (`#(ok ,r) (erlang-> r))
      (err err))))
```

**Benefits:**
- Binary method dispatch (30-40% faster)
- Single-pass conversions
- Reduced allocations
- Cleaner code structure

---

### 6. `http.status.lfe` (Status Codes)
**Current Issues:**
- Function-based access (overhead)
- Novelty codes pollute namespace

**Changes:**
```lfe
; KEEP: Current function-based approach (it's fine)
; BUT ADD: Direct integer constants for common cases

(defmacro OK () 200)
(defmacro CREATED () 201)
(defmacro NO-CONTENT () 204)
(defmacro BAD-REQUEST () 400)
(defmacro UNAUTHORIZED () 401)
(defmacro FORBIDDEN () 403)
(defmacro NOT-FOUND () 404)
(defmacro ERROR () 500)
(defmacro BAD-GATEWAY () 502)
(defmacro UNAVAILABLE () 503)

; NEW: Fast status validation
(defun valid? (code)
  (and (is_integer code) 
       (>= code 100) 
       (<= code 599)))

; NEW: Status text lookup (cached map)
(defun text (code)
  (maps:get code (status-text-map) #"Unknown"))

; OPTIONAL: Move 7xx novelty codes to separate module
; http.status.fun or http.status.novelty
```

**Benefits:**
- Macro constants (compile-time)
- Keep existing API for compatibility
- Fast validation

---

### 7. `http.util.lfe` (Utilities)
**Current Issues:**
- Minimal functionality
- Could be more comprehensive

**Changes:**
```lfe
; KEEP: Existing functions but optimize

(defun add-header (req-or-resp key val)
  (let* ((hs (mref req-or-resp 'headers))
         (key-bin (ensure-binary key))
         (val-bin (ensure-binary val)))
    (mset req-or-resp 'headers (maps:put key-bin val-bin hs))))

(defun http-version-tuple
  ((1.0) #(1 0))
  ((1.1) #(1 1))
  ((2) #(2 0))
  ((3) #(3 0))
  ((v) v))

; NEW: Common utilities
(defun ensure-binary
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (iolist_to_binary l))
  ((a) (when (is_atom a)) (atom_to_binary a))
  ((i) (when (is_integer i)) (integer_to_binary i)))

(defun binary-upcase (bin)
  (list_to_binary (string:uppercase (binary_to_list bin))))

(defun binary-downcase (bin)
  (list_to_binary (string:lowercase (binary_to_list bin))))

(defun binary-downcase-atom (bin)
  (binary_to_atom (binary-downcase bin)))

; NEW: URL utilities
(defun join-path (segments)
  (iolist_to_binary 
    (lists:join #"/" segments)))

(defun query-string (params)
  ; Fast query string encoding
  )

; NEW: Performance helpers
(defun measure (fun)
  ; Microsecond timing for benchmarking
  (let ((start (erlang:monotonic_time 'microsecond)))
    (let ((result (funcall fun)))
      (let ((elapsed (- (erlang:monotonic_time 'microsecond) start)))
        `#(,result ,elapsed)))))
```

**Benefits:**
- Centralized conversion logic
- Reduced code duplication
- Better benchmarking support

---

### 8. NEW: `http.mimetype.lfe` (MIME Types)
**Current Issues:**
- Reference in http.c to `http.mimetype:text/html` but module doesn't exist!

**Changes:**
```lfe
; NEW: Fast MIME type constants (compile-time)
(defmacro text/plain () #"text/plain; charset=utf-8")
(defmacro text/html () #"text/html; charset=utf-8")
(defmacro application/json () #"application/json; charset=utf-8")
(defmacro application/xml () #"application/xml; charset=utf-8")
(defmacro application/octet-stream () #"application/octet-stream")
(defmacro multipart/form-data () #"multipart/form-data")

; NEW: Extension to MIME mapping (pre-compiled map)
(defun from-extension (ext)
  (maps:get ext (mime-map) (application/octet-stream)))

(defun mime-map ()
  #m(#"html" #"text/html"
     #"htm" #"text/html"
     #"json" #"application/json"
     #"xml" #"application/xml"
     #"txt" #"text/plain"
     #"css" #"text/css"
     #"js" #"application/javascript"
     #"png" #"image/png"
     #"jpg" #"image/jpeg"
     #"jpeg" #"image/jpeg"
     #"gif" #"image/gif"
     #"svg" #"image/svg+xml"
     #"pdf" #"application/pdf"))
```

---

## Performance Optimizations

### 1. **Inline Directives**
```lfe
; Mark hot-path functions for inlining
(compile (inline ensure-binary 1))
(compile (inline valid-method? 1))
(compile (inline binary-upcase 1))
```

### 2. **Binary Pattern Matching**
```lfe
; OLD: Atom matching
(case method
  ('get ...)
  ('post ...))

; NEW: Binary matching (faster)
(case method
  (#"GET" ...)
  (#"POST" ...))
```

### 3. **Reduce Allocations**
```lfe
; OLD: Multiple intermediate lists
(maps:from_list (lists:map #'f/1 (lists:sort list)))

; NEW: Single pass with fold
(lists:foldl #'fold-fn/2 #m() list)
```

### 4. **Pre-compute Constants**
```lfe
; Compile-time computation
(defmacro default-headers ()
  ; Computed at compile time, not runtime
  ...)
```

---

## Breaking Changes Summary

### API Changes
1. **Methods**: Binaries instead of atoms
   - OLD: `'get`, `'post`
   - NEW: `#"GET"`, `#"POST"`

2. **Headers**: All binary keys/values
   - OLD: Mixed atoms/strings/binaries
   - NEW: Binary-only

3. **Request/Response Maps**: Standardized structure
   - All keys are atoms (no change)
   - All values are binaries where applicable

4. **Function Signatures**: Some argument order changes for consistency

### Removed APIs
- `http.request:->list/1` (debugging only, move to separate module)
- Excessive conversion helpers (consolidated)

### New APIs
- `http.response:json/2` - Fast JSON responses
- `http.response:ok/1`, `not-found/1`, etc. - Quick builders
- `http.header:get-ci/2` - Case-insensitive lookup
- `http.mimetype` module - MIME type handling
- `http.util:measure/1` - Performance measurement

---

## Migration Guide

### For Users

```lfe
; OLD v0.5.4
(http.c:request 'get "http://example.com")
(http.request:new 'post url body headers)
(http.header:add headers "Content-Type" "application/json")

; NEW v1.0.0
(http.c:request #"GET" "http://example.com")
(http.request:new #"POST" url body headers)
(http.header:add headers #"Content-Type" #"application/json")

; OR use new convenience functions
(http.c:request (method-get) "http://example.com")
```

### Quick Migration Script
```lfe
; Replace atom methods with binaries
's/'get/\#"GET"/g
's/'post/\#"POST"/g
's/'put/\#"PUT"/g
's/'delete/\#"DELETE"/g

; Update header keys to binaries
's/"\\([^"]*\\)"/\#"\\1"/g (in header contexts)
```

---

## Implementation Order

1. **Phase 1**: Core modules (http.lfe, http.util.lfe)
2. **Phase 2**: Data structures (http.header.lfe, http.status.lfe)
3. **Phase 3**: Request/Response (http.request.lfe, http.response.lfe)
4. **Phase 4**: Interop (http.c.lfe)
5. **Phase 5**: New modules (http.mimetype.lfe)
6. **Phase 6**: Documentation and migration guide
7. **Phase 7**: Benchmarks and optimization validation

---

## Benchmarking Plan

Create `bench/` directory with:
- `header-bench.lfe` - Header operations
- `request-bench.lfe` - Request building
- `response-bench.lfe` - Response building
- `client-bench.lfe` - Full request/response cycle
- `comparison.lfe` - v0.5.4 vs v1.0.0

Target improvements:
- Header operations: 50-70% faster
- Request building: 40-60% fewer allocations
- Method dispatch: 30-40% faster
- Full cycle: 25-35% overall improvement

---

## Testing Strategy

1. **Unit tests** for each module
2. **Property-based tests** for conversions
3. **Integration tests** with real HTTP servers
4. **Performance regression tests**
5. **Backward compatibility tests** (with warnings)

---

## Documentation Updates

1. **README**: New API examples, migration guide
2. **CHANGELOG**: Detailed breaking changes
3. **UPGRADING**: Step-by-step migration
4. **Performance**: Benchmark results
5. **API docs**: Updated signatures and examples

---

## Success Metrics

- [ ] 30%+ performance improvement in benchmarks
- [ ] 40%+ reduction in allocations
- [ ] All tests passing
- [ ] Migration guide complete
- [ ] Zero compiler warnings
- [ ] Documentation updated
- [ ] Hex package published

---

## Timeline Estimate

- **Week 1**: Core modules + testing
- **Week 2**: Data structures + benchmarks
- **Week 3**: Request/Response + optimization
- **Week 4**: Interop + edge cases
- **Week 5**: Documentation + migration tools
- **Week 6**: Final testing + release prep

**Total**: 6 weeks for complete v1.0.0 release

---

## Risk Mitigation

1. **Breaking changes**: Clear migration path, maybe provide shim layer
2. **Performance regressions**: Continuous benchmarking
3. **Erlang interop issues**: Comprehensive testing with httpc
4. **Binary matching bugs**: Property-based testing
5. **User adoption**: Detailed examples, migration script

---

## Future Enhancements (Post-1.0.0)

- HTTP/2 support
- Streaming responses
- Connection pooling
- Async request API
- Middleware system
- Request/response interceptors
- Built-in caching layer
- WebSocket support (http.ws module)

---

## Questions to Resolve

1. Should we provide a compatibility shim for v0.5.x API?
2. Keep novelty status codes (7xx) or move to separate package?
3. Add streaming support in v1.0.0 or defer to v1.1.0?
4. Include JSON encoder/decoder or keep as external dependency?
5. Support for HTTP/2 in initial release or add later?

**Recommendation**: Ship lean v1.0.0 focused on performance, add features in 1.x releases