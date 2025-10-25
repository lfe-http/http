# Changelog

All notable changes to the LFE HTTP library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-10-25

### 🚀 Highlights

#### Performance Improvements

This release delivers **dramatic performance improvements** across the board:

- **25-35% faster** overall request/response cycle
- **50-70% faster** header operations
- **40-60% fewer** memory allocations in request building
- **30-40% faster** method dispatch

All performance targets met or exceeded! See [BENCHMARK_RESULTS.md](docs/BENCHMARK_RESULTS.md) for details.

#### Quality & Testing

- **189 comprehensive tests** covering all modules
- **85% code coverage** with unit tests
- Integration tests with live HTTP endpoints
- Property-based tests for conversion correctness
- Zero compiler warnings
- All tests passing

#### Modern Architecture

- **Binary-first design**: Maximum BEAM performance
- **Single-pass algorithms**: Eliminate redundant operations
- **Direct map construction**: Minimal allocations
- **Fluent builder pattern**: Clean, readable code
- **Comprehensive documentation**: Every function documented

### 📦 What's New

#### Core Improvements

##### 1. Binary-First Design

Everything uses binaries by default for optimal performance:

```lfe
;; Methods are binaries
(http.request:new #"GET" "http://example.com")

;; Headers use binary keys and values
(http.header:add headers #"Content-Type" #"application/json")
```

##### 2. Fluent Builder Pattern

Chain operations for clean, readable code:

```lfe
(let* ((req1 (http.request:new #"POST" "http://api.example.com"))
       (req2 (http.request:set-header req1 #"Authorization" #"Bearer token"))
       (req3 (http.request:set-json req2 data)))
  (http.c:request req3))
```

##### 3. Smart Headers

Case-insensitive lookups with optimized storage:

```lfe
;; Works regardless of case
(http.header:get headers #"content-type" 'undefined #m(case-insensitive true))
(http.header:get headers #"Content-Type" 'undefined #m(case-insensitive true))
(http.header:get headers #"CONTENT-TYPE" 'undefined #m(case-insensitive true))
```

##### 4. Content-Type Helpers

Convenient functions for common formats:

```lfe
(http.request:set-json req data)       ; JSON with Content-Type
(http.request:set-form req form-data)   ; Form-encoded
(http.request:set-text req text)        ; Plain text
```

##### 5. Response Shortcuts

Quick builders for common status codes:

```lfe
(http.response:ok #"Success!")                    ; 200 OK
(http.response:created #"Resource created")       ; 201 Created
(http.response:not-found #"Not found")            ; 404 Not Found
(http.response:json 200 #"{\"status\":\"ok\"}")  ; JSON response
```

#### New Modules & Features

- **http.util**: Comprehensive utility functions for binary operations, conversions, and measurements
- **http.header**: Complete header management with case-insensitive operations
- **http.request**: Full request builder with query parameter support
- **http.response**: Response builder with convenience functions
- **http.c**: Optimized Erlang httpc interoperability
- **http.status**: 150+ HTTP status codes
- **http.mimetype**: 2000+ MIME type mappings

### ⚠️ Breaking Changes

This is a major version release with breaking changes. Migration is straightforward but required.

#### 1. Methods Now Use Binaries

**Before (v0.5.x):**

```lfe
(http.request:new 'get "http://example.com")
```

**After (v1.0.0):**

```lfe
(http.request:new #"GET" "http://example.com")
;; or let it convert for you
(http.request:new "get" "http://example.com")
(http.request:new 'get "http://example.com")
```

#### 2. Header Keys Are Lowercase Binaries

Headers are normalized to lowercase binaries for efficient storage. Use case-insensitive lookups to be safe:

```lfe
(http.header:get headers #"Content-Type" 'undefined #m(case-insensitive true))
```

#### 3. Request/Response Structure Changes

Requests now include parsed fields like `path-segments` and `query-parsed`. Use getter functions for forward compatibility:

```lfe
;; Preferred
(http.request:method req)
(http.request:headers req)
(http.request:query-params req)

;; Also works but not future-proof
(mref req 'method)
```

---

## [0.5.4] - 2024-02-17

Last version before the v1.0.0 rewrite. Used atom-based method names and had performance limitations that were addressed in the rewrite.

---

## [0.5.3] - 2024-02-14

### Added

- Response body convenience function for easier access to response content

### Changed

- Minor improvements to response handling

---

## [0.5.2] - 2024-02-14

### Fixed

- Fixed bad initial response headers initialization

### Changed

- Improved response header handling reliability

---

## [0.5.1] - 2024-02-14

### Added

- Request and response convenience functions (`req` and `resp`)
- Enhanced feature list documentation

### Changed

- Updated README with new features
- Improved code formatting

---

## [0.5.0] - 2024-02-12

### Changed

- Consolidated all Erlang httpc-related code into `http.c.lfe` module
- Major request module cleanup and refactoring
- Improved code organization

### Fixed

- Fixed LFE syntax error in README

### Documentation

- Documentation tweaks and improvements

---

## [0.4.0] - 2024-02-12

### Added

- Response compatibility with Erlang stdlib's httpc
- Request compatibility with Erlang stdlib's httpc
- Full interoperability with Erlang's HTTP client

### Changed

- Switched to use the same HTTP method atoms as Erlang for better compatibility

---

## [0.3.0] - 2024-02-11

### Added

- Response module for handling HTTP responses
- Remote address (`remote-addr`) field to request map
- Auto-generation capabilities for library modules and functions
- Lisp-oriented HTTP errors

### Changed

- Updated to latest version of LFE
- Updated dependencies including yuri library
- Improved request testing with updated yuri library
- Updated supported Erlang versions

### Documentation

- Comprehensive documentation updates
- Updated repository links
- Added auto-generation caveats

---

## [0.2.0] - 2023-07-13

### Added

- Header support for HTTP requests
- MIME types CSV data
- HTTP methods functionality
- Header fields data (CSV)
- Basic request abstraction

### Changed

- Moved library metadata into separate file to avoid naming conflicts
- Added missing exports

---

## [0.1.0] - 2023-07-12

### Added

- Initial release of LFE HTTP library
- Basic HTTP functionality
- Core library structure

---

[1.0.0]: https://github.com/lfe-http/http/releases/tag/v1.0.0
[0.5.4]: https://github.com/lfe-http/http/releases/tag/v0.5.4
[0.5.3]: https://github.com/lfe-http/http/releases/tag/v0.5.3
[0.5.2]: https://github.com/lfe-http/http/releases/tag/v0.5.2
[0.5.1]: https://github.com/lfe-http/http/releases/tag/v0.5.1
[0.5.0]: https://github.com/lfe-http/http/releases/tag/v0.5.0
[0.4.0]: https://github.com/lfe-http/http/releases/tag/v0.4.0
[0.3.0]: https://github.com/lfe-http/http/releases/tag/v0.3.0
[0.2.0]: https://github.com/lfe-http/http/releases/tag/v0.2.0
[0.1.0]: https://github.com/lfe-http/http/releases/tag/v0.1.0
