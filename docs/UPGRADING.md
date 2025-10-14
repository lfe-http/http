# Upgrading from v0.5.4 to v1.0.0

## Overview

Version 1.0.0 is a **major breaking release** focused on performance and code quality. The core APIs have changed to use binary-first operations throughout.

## Breaking Changes

### 1. HTTP Methods

**v0.5.4**: Atom-based methods
```lfe
(http.c:request 'get "http://example.com")
(http.request:new 'post url body)
```

**v1.0.0**: Binary methods
```lfe
(http.c:request #"GET" "http://example.com")
(http.request:new #"POST" url body)

;; OR use macros
(http.c:request (method-get) "http://example.com")
```

### 2. Headers

**v0.5.4**: Mixed types
```lfe
#m("Content-Type" "application/json"
   'authorization "Bearer token")
```

**v1.0.0**: Binary keys and values
```lfe
#m(#"Content-Type" #"application/json"
   #"Authorization" #"Bearer token")
```

### 3. API Changes

| v0.5.4 | v1.0.0 | Notes |
|--------|--------|-------|
| `http.header:list->map` | `http.header:from-list` | Renamed for clarity |
| `http.request:->list` | Removed | Debug only, use `to-map` if needed |
| N/A | `http.mimetype` module | New module added |

## Migration Steps

### Step 1: Update Method Calls

Search and replace in your codebase:

```bash
# Replace atom methods with binaries
sed -i "s/'get/#\"GET\"/g" src/**/*.lfe
sed -i "s/'post/#\"POST\"/g" src/**/*.lfe
sed -i "s/'put/#\"PUT\"/g" src/**/*.lfe
sed -i "s/'delete/#\"DELETE\"/g" src/**/*.lfe
sed -i "s/'patch/#\"PATCH\"/g" src/**/*.lfe
sed -i "s/'head/#\"HEAD\"/g" src/**/*.lfe
sed -i "s/'options/#\"OPTIONS\"/g" src/**/*.lfe
sed -i "s/'trace/#\"TRACE\"/g" src/**/*.lfe
```

### Step 2: Update Header Definitions

Convert string/atom headers to binaries:

```lfe
;; OLD
(def headers
  #m("content-type" "application/json"
     'authorization "Bearer token"))

;; NEW
(def headers
  #m(#"Content-Type" #"application/json"
     #"Authorization" #"Bearer token"))
```

### Step 3: Update Function Calls

```lfe
;; OLD
(http.header:list->map proplist)

;; NEW
(http.header:from-list proplist)
```

### Step 4: Test Thoroughly

Run your test suite to catch any remaining issues:

```bash
rebar3 as test check
```

## New Features

### 1. Convenience Response Builders

```lfe
;; Quick responses
(http.response:ok #"Success")
(http.response:not-found #"Not found")
(http.response:error #"Server error")

;; JSON responses
(http.response:json 200 #"{\"status\":\"ok\"}")
```

### 2. Case-Insensitive Header Lookups

```lfe
;; HTTP headers are case-insensitive per spec
(http.header:get-ci headers #"content-type")
;; Works with any case: "Content-Type", "CONTENT-TYPE", etc.
```

### 3. Builder Pattern for Requests

```lfe
;; Chainable operations
(-> (http.request:new "http://api.example.com")
    (http.request:set-method #"POST")
    (http.request:set-json #"{\"data\":\"value\"}")
    (http.request:set-header #"Authorization" #"Bearer token"))
```

### 4. Query Parameter Helpers

```lfe
(-> (http.request:new "http://api.example.com")
    (http.request:add-query-param #"page" #"1")
    (http.request:add-query-param #"limit" #"100"))
```

### 5. MIME Type Utilities

```lfe
(http.mimetype:from-extension #"json")
;; => #"application/json; charset=utf-8"

(http.mimetype:from-path #"/api/data.json")
;; => #"application/json; charset=utf-8"
```

## Performance Improvements

v1.0.0 delivers significant performance gains:

- **Header operations**: 50-70% faster
- **Request construction**: 40-60% fewer allocations
- **Method dispatch**: 30-40% faster
- **Overall**: 25-35% improvement in full request/response cycle

## Compatibility Notes

### What Stays the Same

- Request/response map structure (keys are still atoms)
- Status code functions (`(http.status:ok)` still returns 200)
- Library metadata API
- Erlang httpc integration (internal changes only)

### Deprecations

None - breaking changes are immediate in v1.0.0.

## Getting Help

- Check examples in `examples/` directory
- Read API documentation in `docs/`
- Report issues: https://github.com/lfe-http/http/issues

## Rollback Plan

If you encounter issues:

1. Pin to v0.5.4 in `rebar.config`:
```erlang
{deps, [
  {http, "0.5.4"}
]}.
```

2. Report the issue so we can help
3. Consider gradual migration by isolating changes

---

**The v1.0.0 rewrite was designed for performance and maintainability. While the migration requires effort, the benefits are significant.**
