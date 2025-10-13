# Phase 0: LFE HTTP Library v1.0.0 - Development Context

## Purpose
This document serves as the **master reference** for all development phases. Read this whenever context is reset or you need to understand the overall project.

---

## Project Overview

### What We're Building
A complete rewrite of the LFE HTTP library (v0.5.4 → v1.0.0) focused on:
- **Performance**: 25-35% overall improvement, 50-70% faster header operations
- **Binary-first architecture**: Eliminate unnecessary conversions
- **Zero-copy operations**: Pass references, not copies
- **Idiomatic LFE**: Leverage pattern matching, guards, macros
- **Breaking changes are expected and acceptable**

### Key Principles
1. **Binary-First**: Use `#"GET"` not `'get`, `#"Content-Type"` not `"Content-Type"`
2. **Single-Pass**: Avoid multiple conversions (atom→list→binary becomes direct)
3. **Inline Hot Paths**: Mark critical functions with `(compile (inline ...))`
4. **Pre-compute**: Use macros for compile-time constants
5. **Direct Construction**: Build maps directly, avoid intermediate structures

---

## Current Codebase (v0.5.4)

### File Structure
```
src/
├── http.app.src          # Application metadata
├── http.lfe              # Core API (methods, versions, defaults)
├── http.c.lfe            # Erlang httpc interop (COMPLEX, SLOW)
├── http.header.lfe       # Header management (CONVERSION HEAVY)
├── http.lib.lfe          # Library metadata
├── http.request.lfe      # Request building (MULTIPLE PASSES)
├── http.response.lfe     # Response building
├── http.status.lfe       # HTTP status codes
├── http.util.lfe         # Utilities
└── http.vsn.lfe          # Version info
```

### Major Problems
1. **Atom-based method matching**: `'get`, `'post` → slow case statements
2. **Excessive conversions**: `kv->bins` does atom→list→binary recursively
3. **Multiple allocations**: Request building does 3-4 `maps:merge` calls
4. **Missing module**: `http.mimetype` referenced but doesn't exist!
5. **String headers**: Mixed types cause conversion overhead

---

## Architecture Changes

### Method Representation
```lfe
; OLD (v0.5.4)
(http.c:request 'get "http://example.com")
(case method ('get ...) ('post ...))

; NEW (v1.0.0)
(http.c:request #"GET" "http://example.com")
(case method (#"GET" ...) (#"POST" ...))

; OR use macros
(http.c:request (method-get) "http://example.com")
```

### Header Representation
```lfe
; OLD (v0.5.4)
#m("Content-Type" "application/json"
   'authorization "Bearer token")

; NEW (v1.0.0)
#m(#"Content-Type" #"application/json"
   #"Authorization" #"Bearer token")
```

### Request/Response Structure (Minimal Changes)
```lfe
; Request map structure (keys stay as atoms)
#m(method #"GET"              ; BINARY method
   version 1.1                ; Float version
   headers #m(...)            ; Binary key/val map
   body #""                   ; Binary body
   url #"http://..."          ; Binary URL
   url-parsed #m(...)         ; Parsed URL map
   path-segments (...)        ; List of binary segments
   query-parsed #m(...))      ; Binary key/val map

; Response map structure
#m(status 200                 ; Integer status
   headers #m(...)            ; Binary key/val map
   body #""                   ; Binary body
   version 1.1)               ; Float version
```

---

## Development Phases

### Phase 1: Core Infrastructure (2-3 hours)
**Files**: `http.util.lfe`, `http.lfe`, `http.mimetype.lfe` (NEW)

**Creates foundation**:
- Binary conversion utilities (`ensure-binary`, `binary-upcase`, etc.)
- Method macros (`method-get`, `method-post`, etc.)
- MIME type constants
- Inline directives for hot paths

**Why first**: Everything depends on these utilities

---

### Phase 2: Header Management (2-3 hours)
**Files**: `http.header.lfe`

**Replaces**:
- Recursive `kv->bins` → single-pass fold
- `list->map` → optimized `from-list`
- Adds case-insensitive lookups

**Depends on**: Phase 1 (needs `ensure-binary`)

---

### Phase 3: Request Builder (3-4 hours)
**Files**: `http.request.lfe`

**Replaces**:
- Multiple `maps:merge` → single construction
- Removes `->list` (debug only)
- Adds threading macro style
- Efficient multi-arity constructors

**Depends on**: Phase 1, Phase 2

---

### Phase 4: Response Builder (2-3 hours)
**Files**: `http.response.lfe`

**Adds**:
- Fast response helpers (`ok`, `not-found`, `json`, etc.)
- Optimized `set-body`
- Direct map construction

**Depends on**: Phase 1, Phase 2

---

### Phase 5: Status Codes (1-2 hours)
**Files**: `http.status.lfe`

**Adds**:
- Status code macros for common codes
- Validation function
- Status text lookup map
- Keeps existing function API

**Depends on**: Phase 1

---

### Phase 6: Erlang Interop (3-4 hours)
**Files**: `http.c.lfe`

**Replaces**:
- Atom method dispatch → binary pattern matching
- Multiple conversion passes → single pass
- Complex case statements → direct pattern match

**Depends on**: Phase 1, 2, 3, 4 (needs everything)

---

### Phase 7: Testing & Benchmarking (4-5 hours)
**Creates**: `test/`, `bench/` directories

**Includes**:
- Unit tests for all modules
- Property-based tests for conversions
- Integration tests with httpc
- Performance benchmarks
- Regression tests

**Depends on**: All implementation phases

---

### Phase 8: Documentation (2-3 hours)
**Creates**: `README.md`, `UPGRADING.md`, `MIGRATION.md`, `docs/`

**Includes**:
- API documentation
- Migration guide (v0.5.4 → v1.0.0)
- Performance benchmarks results
- Code examples

**Depends on**: All phases complete

---

## Critical Implementation Rules

### 1. Binary Patterns
```lfe
; ALWAYS use binary patterns for HTTP methods
(defun handle-method
  ((#"GET" req) ...)
  ((#"POST" req) ...)
  ((#"PUT" req) ...))

; NOT atom patterns
; (('get req) ...) ; ❌ WRONG
```

### 2. Single-Pass Conversions
```lfe
; GOOD: Single fold operation
(lists:foldl 
  (lambda (kv acc) 
    (let ((`#(,k ,v) (normalize-kv kv)))
      (maps:put k v acc)))
  #m()
  proplist)

; BAD: Multiple passes
; (maps:from_list (list->bins (lists:map #'convert/1 proplist))) ; ❌
```

### 3. Inline Directives
```lfe
; Mark hot-path functions
(compile (inline ensure-binary 1))
(compile (inline valid-method? 1))
(compile (inline binary-upcase 1))
```

### 4. Direct Map Construction
```lfe
; GOOD: Direct construction
(defun new (url)
  #m(method #"GET"
     version 1.1
     headers (http:default-headers)
     url url))

; BAD: Multiple merges
; (maps:merge (maps:merge base-map method-map) header-map) ; ❌
```

### 5. Guard Usage
```lfe
; Use guards for type checking
(defun ensure-binary
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (iolist_to_binary l))
  ((a) (when (is_atom a)) (atom_to_binary a)))
```

---

## Testing Strategy

### For Each Module
1. **Unit tests**: Test every exported function
2. **Property tests**: Verify conversions are reversible
3. **Performance tests**: Benchmark against v0.5.4
4. **Integration tests**: Test with real HTTP calls

### Test File Naming
```
test/
├── http-tests.lfe
├── http-header-tests.lfe
├── http-request-tests.lfe
├── http-response-tests.lfe
├── http-c-tests.lfe
└── http-status-tests.lfe

bench/
├── header-bench.lfe
├── request-bench.lfe
├── response-bench.lfe
└── full-cycle-bench.lfe
```

---

## Performance Targets

### By Module
- **http.header**: 50-70% faster header operations
- **http.request**: 40-60% fewer allocations
- **http.c**: 30-40% faster method dispatch
- **Overall**: 25-35% improvement in full request cycle

### How to Measure
```lfe
; Use http.util:measure/1
(let ((`#(,result ,elapsed-us) (http.util:measure (lambda () (do-work)))))
  (io:format "Completed in ~pμs~n" (list elapsed-us))
  result)
```

---

## Common Pitfalls to Avoid

### 1. Forgetting Binary Literals
```lfe
; ❌ WRONG
(maps:put "Content-Type" "application/json" headers)

; ✅ CORRECT
(maps:put #"Content-Type" #"application/json" headers)
```

### 2. Using Atoms for HTTP Methods
```lfe
; ❌ WRONG
(case method ('get ...) ('post ...))

; ✅ CORRECT
(case method (#"GET" ...) (#"POST" ...))
```

### 3. Multiple Conversions
```lfe
; ❌ WRONG
(defun process (val)
  (binary_to_list (list_to_binary (atom_to_list val))))

; ✅ CORRECT
(defun process (val)
  (atom_to_binary val))
```

### 4. Not Using Guards
```lfe
; ❌ WRONG (will crash on wrong types)
(defun ensure-binary (x)
  (if (is_binary x) x (list_to_binary x)))

; ✅ CORRECT (explicit pattern matching)
(defun ensure-binary
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (list_to_binary l)))
```

### 5. Ignoring Inlining Opportunities
```lfe
; ❌ WRONG (missing inline directive)
(defun hot-function (x) ...)

; ✅ CORRECT
(compile (inline hot-function 1))
(defun hot-function (x) ...)
```

---

## Code Style Guidelines

### 1. Function Ordering
```lfe
; 1. Module declaration
(defmodule http.example ...)

; 2. Compiler directives
(compile (inline func 1))

; 3. Public API (exported functions)
(defun public-func ...)

; 4. Private helpers
(defun private-helper ...)
```

### 2. Pattern Matching Style
```lfe
; Use multi-clause functions, not nested cases
(defun process
  ((#"GET" data) ...)
  ((#"POST" data) ...)
  ((method data) ...))
```

### 3. Documentation
```lfe
; Every exported function needs a docstring
(defun add-header (headers key val)
  "Add a header key-value pair to the headers map.
  
  Args:
    headers: Map of binary header keys to binary values
    key: Header name (binary, string, or atom - will be converted)
    val: Header value (binary, string, or atom - will be converted)
    
  Returns:
    Updated headers map"
  ...)
```

### 4. Naming Conventions
- Functions: `kebab-case` (e.g., `add-header`, `ensure-binary`)
- Predicates: End with `?` (e.g., `valid-method?`, `binary?`)
- Converters: Use `->` (e.g., `erlang->`, `->erlang`)
- Macros: `SCREAMING-KEBAB` or `kebab-case` (e.g., `METHOD-GET` or `method-get`)

---

## Dependencies

### External Libraries
- **yuri**: URL parsing (already in use)
- **Erlang stdlib**: `httpc`, `maps`, `lists`, etc.
- **LFE standard library**: Core functionality

### Version Compatibility
- **LFE**: 2.0+
- **Erlang/OTP**: 24+
- **rebar3**: 3.18+

---

## Version Numbers

### Current
- v0.5.4 (production)

### Target
- v1.0.0 (breaking release)

### Future
- v1.1.0: Streaming support
- v1.2.0: HTTP/2 support
- v2.0.0: HTTP/3 support

---

## Success Criteria

### Must Have (v1.0.0)
- ✅ All modules rewritten with binary-first approach
- ✅ Performance targets met (25-35% improvement)
- ✅ All tests passing
- ✅ Migration guide complete
- ✅ API documentation updated
- ✅ Zero compiler warnings

### Nice to Have (v1.0.0)
- ⭐ Compatibility shim for v0.5.4 API
- ⭐ Automated migration script
- ⭐ Visual performance comparisons
- ⭐ Live examples/playground

### Future (v1.x)
- 🔮 Streaming response support
- 🔮 Connection pooling
- 🔮 Middleware system
- 🔮 HTTP/2 support
- 🔮 WebSocket support

---

## Quick Reference

### When You Need to...

**Start a new phase**:
1. Read this document (Phase 0)
2. Read the specific phase document
3. Review dependencies (completed phases)
4. Implement according to specification
5. Write tests
6. Run benchmarks
7. Update documentation

**Reset context**:
1. Read this document first
2. Check which phase you're on
3. Review completed phases
4. Read current phase document
5. Continue implementation

**Debug an issue**:
1. Check "Common Pitfalls" section
2. Verify binary literals used correctly
3. Check pattern matching guards
4. Run tests for affected module
5. Compare with v0.5.4 behavior if needed

**Optimize performance**:
1. Add inline directives
2. Eliminate conversions
3. Use binary pattern matching
4. Profile with `http.util:measure/1`
5. Compare with benchmarks

---

## Contact & Resources

### Documentation
- LFE Docs: https://lfe.io/
- Erlang Docs: https://erlang.org/doc/
- Project Repo: https://github.com/lfe-http/http

### Getting Help
- Review Phase 0 (this document)
- Check specific phase instructions
- Examine existing code patterns
- Refer to LFE/Erlang documentation

---

## Version History

- **v0.1** (2025-01-15): Initial development plan created
- **v0.2** (2025-01-15): Phase breakdown finalized
- **v1.0** (TBD): Development complete, ready for release

---

**Remember**: This is a breaking release. Prioritize performance and clean architecture over backward compatibility. The migration path is well-documented, so users can upgrade smoothly.