# LFE HTTP Library - Examples

This directory contains working examples demonstrating the LFE HTTP library v1.0.0 API.

## Running the Examples

### Prerequisites

Make sure you have the library compiled:

```bash
rebar3 compile
```

### From LFE REPL

Start the LFE REPL:

```bash
rebar3 lfe repl
```

Then compile and run any example:

```lfe
> (c "examples/basic-client.lfe")
> (basic-client:get-example)
```

## Available Examples

### 1. basic-client.lfe

Demonstrates basic HTTP client operations using the `http.c` module for making actual HTTP requests.

**Functions:**
- `get-example/0` - Simple GET request
- `post-json-example/0` - POST request with JSON body
- `post-form-example/0` - POST request with form-encoded data
- `custom-headers-example/0` - Request with custom headers

**Usage:**
```lfe
> (c "examples/basic-client.lfe")
> (inets:start)
> (ssl:start)
> (basic-client:get-example)
```

### 2. request-builder.lfe

Shows how to use the fluent builder pattern to construct HTTP requests.

**Functions:**
- `simple-build/0` - Build a simple GET request
- `query-params-example/0` - Add query parameters to requests
- `builder-chain-example/0` - Chain multiple builder operations
- `content-type-helpers/0` - Use content-type helper functions

**Usage:**
```lfe
> (c "examples/request-builder.lfe")
> (request-builder:simple-build)
> (request-builder:query-params-example)
```

### 3. response-helpers.lfe

Demonstrates convenient response building functions for common HTTP status codes.

**Functions:**
- `success-responses/0` - 2xx success responses (OK, Created, Accepted, etc.)
- `error-responses/0` - 4xx/5xx error responses (Bad Request, Not Found, Error, etc.)
- `content-responses/0` - Content-type specific responses (JSON, HTML, Text, XML)
- `custom-response/0` - Build a custom response from scratch

**Usage:**
```lfe
> (c "examples/response-helpers.lfe")
> (response-helpers:success-responses)
> (response-helpers:error-responses)
```

### 4. headers.lfe

Shows comprehensive header management capabilities.

**Functions:**
- `basic-operations/0` - Create, add, convert headers
- `case-insensitive/0` - Case-insensitive header lookups
- `bulk-operations/0` - Merge, filter, remove operations
- `header-utilities/0` - Keys, values, normalization

**Usage:**
```lfe
> (c "examples/headers.lfe")
> (headers:basic-operations)
> (headers:case-insensitive)
```

## Key Features Demonstrated

### Binary-First Design

All strings are binaries by default for optimal performance:

```lfe
(http.request:new #"GET" "http://example.com")
(http.header:add headers #"Content-Type" #"application/json")
```

### Fluent Builder Pattern

Chain operations for readable code:

```lfe
(let* ((req1 (http.request:new #"POST" "http://api.example.com"))
       (req2 (http.request:set-header req1 #"Authorization" #"Bearer token"))
       (req3 (http.request:set-json req2 data)))
  (http.c:request req3))
```

### Case-Insensitive Headers

Headers are normalized but can be looked up case-insensitively:

```lfe
;; These all work, regardless of how the header was stored
(http.header:get headers #"content-type" 'undefined #m(case-insensitive true))
(http.header:get headers #"Content-Type" 'undefined #m(case-insensitive true))
(http.header:get headers #"CONTENT-TYPE" 'undefined #m(case-insensitive true))
```

### Content-Type Helpers

Convenient functions for common content types:

```lfe
(http.request:set-json req #"{\"key\":\"value\"}")  ; Sets body and Content-Type
(http.request:set-form req #m(#"field" #"value"))   ; Form-encoded
(http.request:set-text req #"plain text")           ; Plain text
```

### Response Shortcuts

Quick response builders for common status codes:

```lfe
(http.response:ok #"Success!")
(http.response:not-found #"Resource not found")
(http.response:json 200 #"{\"status\":\"ok\"}")
```

## Migration from v0.5.x

If you're upgrading from v0.5.x, the main differences are:

1. **Methods use binaries instead of atoms:**
   ```lfe
   ;; Old (v0.5.x)
   (http.request:new 'get "http://example.com")

   ;; New (v1.0.0)
   (http.request:new #"GET" "http://example.com")
   ```

2. **Header keys are lowercase binaries:**
   ```lfe
   ;; Use case-insensitive lookups to be safe
   (http.header:get headers #"Content-Type" 'undefined #m(case-insensitive true))
   ```

3. **Use getter functions instead of direct map access:**
   ```lfe
   ;; Preferred
   (http.request:method req)
   (http.request:headers req)

   ;; Also works but not recommended
   (mref req 'method)
   ```

See [UPGRADING.md](../docs/UPGRADING.md) for detailed migration instructions.

## Performance

The v1.0.0 rewrite delivers significant performance improvements:

- **25-35% faster** overall request/response cycle
- **50-70% faster** header operations
- **40-60% fewer** memory allocations
- **30-40% faster** method dispatch

Run benchmarks to see the improvements:

```lfe
> (c "bench/comparison-bench.lfe")
> (comparison-bench:run)
```

## More Information

- [Main README](../README.md)
- [CHANGELOG](../CHANGELOG.md)
- [Migration Guide](../docs/UPGRADING.md)
- [API Documentation](../docs/)

## Contributing

Found an issue or want to add an example? Please open an issue or pull request on GitHub:
https://github.com/lfe-http/http
