# Coverage Improvement Plan

**Date**: 2025-10-25
**Current Coverage**: 70%
**Target Coverage**: 85-90%

## Executive Summary

We've successfully improved coverage from 4% to 70% by excluding auto-generated code (`http.mimetype`, `http.status`, `http.lib`, `http.vsn`). Now we need to focus on testing the critical modules with low coverage.

## Current Coverage Breakdown

| Module | Coverage | Lines of Code | Test Lines | Priority | Target |
|--------|----------|---------------|------------|----------|--------|
| `http.request` | 20% | 410 | 46 | **CRITICAL** | 85%+ |
| `http.c` | 38% | 329 | 127 | **CRITICAL** | 80%+ |
| `http.util` | 85% | ~150 | ~120 | Medium | 95% |
| `http` | 90% | ~100 | ~90 | Low | 95% |
| `http.response` | 95% | ~200 | ~180 | Low | 95% |
| `http.header` | 95% | ~180 | ~170 | Low | 95% |

**All test modules**: 100% coverage ✅

## Phase 1: http.request (20% → 85%+)

**Impact**: Will add 15-20% to total coverage

### Currently Tested (5 tests):
- ✅ `new/1` - Basic GET request
- ✅ `new/2` - Method + URL
- ✅ `new/3` - Method + URL + Body
- ✅ `new/4` - Method + URL + Body + Headers
- ✅ Complex URL parsing (auth, port, query, fragment)

### Missing Test Coverage:

#### Setter Functions (NOT TESTED):
1. `set-method/2` - Change HTTP method
2. `set-body/2` - Set request body
3. `set-headers/2` - Replace all headers
4. `set-header/3` - Set single header
5. `add-header/3` - Add header (duplicate handling)
6. `remove-header/2` - Remove header

#### Body Helper Functions (NOT TESTED):
7. `set-json/2` - Set JSON body with Content-Type
8. `set-form/2` - Set form-encoded body
9. `set-text/2` - Set text body

#### Query Parameter Functions (NOT TESTED):
10. `add-query-param/3` - Add single query param
11. `set-query-params/2` - Set all query params

#### Getter Functions (NOT TESTED):
12. `method/1` - Get method from request
13. `url/1` - Get URL from request
14. `body/1` - Get body from request
15. `headers/1` - Get headers from request
16. `path-segments/1` - Get path segments
17. `query-params/1` - Get query params

### Test Implementation Plan:

Create tests in `test/unit/http.request-tests.lfe`:

```lfe
;;; Setter tests
(deftest set-method
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-method req #"POST")))
    (is-equal #"POST" (mref req2 'method))))

(deftest set-body
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-body req #"test data")))
    (is-equal #"test data" (mref req2 'body))))

(deftest set-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-header req #"Content-Type" #"application/json")))
    (is-equal #"application/json"
              (maps:get #"Content-Type" (mref req2 'headers)))))

(deftest add-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:add-header req #"X-Custom" #"value1"))
         (req3 (http.request:add-header req2 #"X-Custom" #"value2")))
    ;; Test duplicate handling
    (is-equal true (maps:is_key #"X-Custom" (mref req3 'headers)))))

(deftest remove-header
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-header req #"X-Custom" #"value"))
         (req3 (http.request:remove-header req2 #"X-Custom")))
    (is-equal false (maps:is_key #"X-Custom" (mref req3 'headers)))))

(deftest set-headers
  (let* ((req (http.request:new "http://example.com/"))
         (headers #m(#"Content-Type" #"application/json"
                     #"Accept" #"application/json"))
         (req2 (http.request:set-headers req headers)))
    (is-equal headers (mref req2 'headers))))

;;; Body helper tests
(deftest set-json
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-json req #"{\"key\":\"value\"}")))
    (is-equal #"{\"key\":\"value\"}" (mref req2 'body))
    (is-equal #"application/json"
              (maps:get #"Content-Type" (mref req2 'headers)))))

(deftest set-form
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-form req #"key=value&foo=bar")))
    (is-equal #"key=value&foo=bar" (mref req2 'body))
    (is-equal #"application/x-www-form-urlencoded"
              (maps:get #"Content-Type" (mref req2 'headers)))))

(deftest set-text
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-text req #"plain text")))
    (is-equal #"plain text" (mref req2 'body))
    (is-equal #"text/plain"
              (maps:get #"Content-Type" (mref req2 'headers)))))

;;; Query parameter tests
(deftest add-query-param
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:add-query-param req #"key1" #"value1"))
         (req3 (http.request:add-query-param req2 #"key2" #"value2")))
    (is-equal #"value1" (maps:get #"key1" (mref req3 'query-parsed)))
    (is-equal #"value2" (maps:get #"key2" (mref req3 'query-parsed)))))

(deftest set-query-params
  (let* ((req (http.request:new "http://example.com/"))
         (params #m(#"key1" #"value1" #"key2" #"value2"))
         (req2 (http.request:set-query-params req params)))
    (is-equal params (mref req2 'query-parsed))))

;;; Getter tests
(deftest method-getter
  (let ((req (http.request:new #"POST" "http://example.com/")))
    (is-equal #"POST" (http.request:method req))))

(deftest url-getter
  (let ((req (http.request:new "http://example.com/")))
    (is-equal #"http://example.com/" (http.request:url req))))

(deftest body-getter
  (let* ((req (http.request:new "http://example.com/"))
         (req2 (http.request:set-body req #"test")))
    (is-equal #"test" (http.request:body req2))))

(deftest headers-getter
  (let* ((req (http.request:new "http://example.com/"))
         (headers #m(#"X-Custom" #"value"))
         (req2 (http.request:set-headers req headers)))
    (is-equal headers (http.request:headers req2))))

(deftest path-segments-getter
  (let ((req (http.request:new "http://example.com/api/v1/users")))
    (is-equal '(#"api" #"v1" #"users") (http.request:path-segments req))))

(deftest query-params-getter
  (let ((req (http.request:new "http://example.com/?key=value")))
    (is-equal #m(#"key" #"value") (http.request:query-params req))))
```

**Estimated new tests**: ~20 tests, ~200 lines
**Expected coverage increase**: 20% → 85%+

---

## Phase 2: http.c (38% → 80%+)

**Impact**: Will add 10-12% to total coverage

### Currently Tested (15 tests):
- ✅ Basic Erlang conversion (GET, POST)
- ✅ Header conversion
- ✅ Method dispatch
- ✅ Response conversion
- ✅ Content-Type extraction
- ✅ Body format options

### Missing Test Coverage:

#### Request Function Arities (PARTIAL):
1. `request/1` - Only basic case tested
2. `request/2` - Method + URL (NOT TESTED)
3. `request/3` - Method + URL + Body (NOT TESTED)
4. `request/4` - Method + URL + Body + Headers (NOT TESTED)
5. `request/5` - With HTTP options (NOT TESTED)
6. `request/6` - With HTTP + general options (NOT TESTED)

#### Error Handling (NOT TESTED):
7. Invalid method handling
8. Error responses from httpc
9. Timeout handling
10. Connection errors

#### Edge Cases (NOT TESTED):
11. Empty headers
12. Binary vs string values
13. Custom HTTP options merging
14. SSL option handling
15. User options override defaults

### Test Implementation Plan:

Create tests in `test/unit/http.c-tests.lfe`:

```lfe
;;; Request arity tests
(deftest request-1-with-url
  (let ((result (http.c:request "https://httpbin.org/get")))
    (is-equal 'ok (element 1 result))))

(deftest request-2-method-url
  (let ((result (http.c:request #"GET" "https://httpbin.org/get")))
    (is-equal 'ok (element 1 result))))

(deftest request-3-with-body
  (let ((result (http.c:request #"POST" "https://httpbin.org/post" #"test")))
    (is-equal 'ok (element 1 result))))

(deftest request-4-with-headers
  (let ((headers #m(#"X-Test" #"value"))
        (result (http.c:request #"POST" "https://httpbin.org/post" #"test" headers)))
    (is-equal 'ok (element 1 result))))

(deftest request-5-with-http-opts
  (let ((http-opts '(#(timeout 5000)))
        (result (http.c:request #"GET" "https://httpbin.org/get" #"" http-opts '())))
    (is-equal 'ok (element 1 result))))

;;; Options merging tests
(deftest default-timeout-applied
  ;; Test that default 60s timeout is applied
  (let* ((req (http.request:new "https://httpbin.org/get"))
         (args (http.c:->erlang req)))
    ;; Check http-options contain default timeout
    (is-equal 'true (lists:keymember 'timeout 1 (lists:nth 3 args)))))

(deftest user-options-override-defaults
  ;; Test that user options override defaults
  (let* ((req (http.request:new "https://httpbin.org/get"))
         (http-opts '(#(timeout 5000)))
         (args (http.c:->erlang req http-opts '())))
    ;; User timeout should override default
    (is-equal 5000 (proplists:get_value 'timeout (lists:nth 3 args)))))

;;; SSL options tests
(deftest ssl-options-included
  (let* ((req (http.request:new "https://httpbin.org/get"))
         (args (http.c:->erlang req)))
    ;; Check ssl options are present
    (is-equal 'true (lists:keymember 'ssl 1 (lists:nth 3 args)))))

;;; Header edge cases
(deftest empty-headers
  (let* ((req (http.request:new "https://httpbin.org/get"))
         (args (http.c:->erlang req))
         (erlang-req (lists:nth 2 args)))
    ;; Empty headers should be empty list
    (is-equal '() (element 2 erlang-req))))

(deftest header-keys-lowercase
  ;; Verify our fix: header keys must be lowercase
  (let* ((req (http.request:new "https://httpbin.org/get"))
         (req2 (http.request:set-header req #"User-Agent" #"test"))
         (args (http.c:->erlang req2))
         (erlang-req (lists:nth 2 args))
         (headers (element 2 erlang-req)))
    ;; Header key should be lowercase string
    (is-equal "user-agent" (element 1 (lists:nth 1 headers)))))
```

**Estimated new tests**: ~15 tests, ~150 lines
**Expected coverage increase**: 38% → 80%+

---

## Phase 3: http.util (85% → 95%)

**Impact**: Will add 2-3% to total coverage

### Strategy:
1. Check coverage HTML report for exact uncovered lines
2. Add targeted tests for missing edge cases
3. Likely missing: error cases, boundary conditions

**Estimated new tests**: ~5-10 tests, ~30-50 lines
**Expected coverage increase**: 85% → 95%

---

## Implementation Order

1. **Phase 1**: http.request tests (highest impact)
2. **Phase 2**: http.c tests (high impact)
3. **Phase 3**: http.util tests (polish)

## Expected Final Results

| Module | Before | After | Delta |
|--------|--------|-------|-------|
| http.request | 20% | 85% | +65% |
| http.c | 38% | 80% | +42% |
| http.util | 85% | 95% | +10% |
| **TOTAL** | **70%** | **87-90%** | **+17-20%** |

## Notes

- All auto-generated modules already excluded from coverage
- Integration tests excluded from coverage (network-dependent)
- Focus is on unit tests that are fast and deterministic
- Coverage HTML reports available at: `_build/unit+test/cover/index.html`

## Success Criteria

- [ ] Total coverage ≥ 85%
- [ ] http.request coverage ≥ 85%
- [ ] http.c coverage ≥ 80%
- [ ] http.util coverage ≥ 95%
- [ ] All new tests pass
- [ ] No regression in existing tests
