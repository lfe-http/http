# Phase 2: Header Management System

## Overview
Rewrite the header management module to use binary-first operations with single-pass conversions. This phase eliminates the recursive `kv->bins` function and implements case-insensitive lookups per HTTP specification.

**Estimated Time**: 2-3 hours  
**Dependencies**: Phase 1 (http.util, http)  
**Modules**: `http.header.lfe`

---

## Objectives

1. ✅ Replace recursive `kv->bins` with single-pass conversion
2. ✅ Implement case-insensitive header lookups (HTTP standard)
3. ✅ Add bulk operations (merge, filter)
4. ✅ Optimize `from-list` for performance
5. ✅ Maintain backward-compatible API
6. ✅ Achieve 50-70% performance improvement

---

## Current Implementation Analysis

### Current State (v0.5.4)
```lfe
(defmodule http.header
  (export
   (add 2) (add 3)
   (list->map 1)
   (new 0)))

(defun add (header-map kv)
  (let ((`#(,k ,v) (kv->bins kv)))
    (maps:put k v header-map)))

(defun add (header-map k v)
  (add header-map `#(,k ,v)))

(defun new ()
  `#m())

(defun list->map (proplist)
  (maps:from_list (list->bins proplist)))

(defun list->bins (proplist)
  (lists:map
   #'kv->bins/1
   proplist))

(defun kv->bins
  ((`#(,k ,v)) (when (andalso (is_binary k) (is_binary v)))
   `#(,k ,v))
  ((`#(,k ,v)) (when (is_list k))
   (kv->bins `#(,(list_to_binary k) ,v)))
  ((`#(,k ,v)) (when (is_list v))
   (kv->bins `#(,k ,(list_to_binary v))))
  ((`#(,k ,v)) (when (is_atom k))
   (kv->bins `#(,(atom_to_list k) ,v)))
  ((`#(,k ,v)) (when (is_atom v))
   (kv->bins `#(,k ,(atom_to_list v))))
  ((`#(,k ,v)) (when (is_integer v))
   (kv->bins `#(,k ,(integer_to_list v 10)))))
```

### Problems Identified
1. **Recursive conversions**: `kv->bins` calls itself multiple times
2. **Intermediate lists**: `list->bins` creates temporary list before `maps:from_list`
3. **No case insensitivity**: HTTP headers are case-insensitive per RFC 7230
4. **Multiple allocation passes**: atom→list→binary chain
5. **Missing operations**: No bulk merge, filter, or common operations

---

## New Implementation

### File: `src/http.header.lfe`

```lfe
(defmodule http.header
  (export
   ;; Legacy API (maintained for compatibility)
   (add 2) (add 3)
   (new 0)
   
   ;; New API (renamed for clarity)
   (from-list 1)        ; Replaces list->map
   
   ;; Case-insensitive operations
   (get 2) (get 3)
   (get-ci 2) (get-ci 3)
   (has-key? 2)
   (has-key-ci? 2)
   
   ;; Bulk operations
   (merge 2)
   (filter 2)
   (remove 2)
   
   ;; Utility operations
   (to-list 1)
   (keys 1)
   (values 1)
   (normalize-key 1)))

;; Compiler directives
(compile (inline normalize-key 1))
(compile (inline normalize-kv 1))

;;; ---------------------------------------------------------------------------
;;; Core API
;;; ---------------------------------------------------------------------------

(defun new
  "Create a new empty headers map.
  
  Returns:
    Empty map ready for binary key-value pairs"
  ()
  #m())

(defun add
  "Add a header using tuple notation.
  Legacy API - maintained for compatibility.
  
  Args:
    headers: Headers map
    kv: Tuple #(key value)
    
  Returns:
    Updated headers map"
  ((headers kv) (when (is_tuple kv))
   (let ((`#(,k ,v) (normalize-kv kv)))
     (maps:put k v headers))))

(defun add
  "Add a header using separate key and value.
  
  Args:
    headers: Headers map
    key: Header name (will be normalized to binary)
    val: Header value (will be converted to binary)
    
  Returns:
    Updated headers map"
  ((headers key val)
   (let ((k (normalize-key key))
         (v (http.util:ensure-binary val)))
     (maps:put k v headers))))

;;; ---------------------------------------------------------------------------
;;; List Conversions
;;; ---------------------------------------------------------------------------

(defun from-list
  "Convert a property list to headers map (single-pass).
  Replaces list->map with optimized implementation.
  
  Args:
    proplist: List of tuples #(key value)
    
  Returns:
    Headers map with binary keys and values"
  ((proplist) (when (is_list proplist))
   ;; Use foldl for single-pass conversion
   (lists:foldl
     (lambda (kv acc)
       (let ((`#(,k ,v) (normalize-kv kv)))
         (maps:put k v acc)))
     #m()
     proplist)))

(defun to-list
  "Convert headers map to property list.
  
  Args:
    headers: Headers map
    
  Returns:
    Sorted list of tuples #(key value)"
  ((headers) (when (is_map headers))
   (lists:sort (maps:to_list headers))))

;;; ---------------------------------------------------------------------------
;;; Lookup Operations
;;; ---------------------------------------------------------------------------

(defun get
  "Get header value (case-sensitive).
  
  Args:
    headers: Headers map
    key: Header name (will be normalized to binary)
    
  Returns:
    Header value or 'undefined"
  ((headers key)
   (get headers key 'undefined)))

(defun get
  "Get header value with default (case-sensitive).
  
  Args:
    headers: Headers map
    key: Header name (will be normalized to binary)
    default: Default value if not found
    
  Returns:
    Header value or default"
  ((headers key default)
   (maps:get (normalize-key key) headers default)))

(defun get-ci
  "Get header value (case-insensitive per HTTP spec).
  Searches for key regardless of case.
  
  Args:
    headers: Headers map
    key: Header name (any case)
    
  Returns:
    Header value or 'undefined"
  ((headers key)
   (get-ci headers key 'undefined)))

(defun get-ci
  "Get header value with default (case-insensitive).
  
  Args:
    headers: Headers map
    key: Header name (any case)
    default: Default value if not found
    
  Returns:
    Header value or default"
  ((headers key default)
   (let ((key-lower (http.util:binary-downcase (http.util:ensure-binary key))))
     (case (find-ci-key headers key-lower)
       ('undefined default)
       (found-key (maps:get found-key headers default))))))

(defun has-key?
  "Check if header exists (case-sensitive).
  
  Args:
    headers: Headers map
    key: Header name
    
  Returns:
    Boolean true/false"
  ((headers key)
   (maps:is_key (normalize-key key) headers)))

(defun has-key-ci?
  "Check if header exists (case-insensitive).
  
  Args:
    headers: Headers map
    key: Header name (any case)
    
  Returns:
    Boolean true/false"
  ((headers key)
   (let ((key-lower (http.util:binary-downcase (http.util:ensure-binary key))))
     (case (find-ci-key headers key-lower)
       ('undefined 'false)
       (_ 'true)))))

;;; ---------------------------------------------------------------------------
;;; Bulk Operations
;;; ---------------------------------------------------------------------------

(defun merge
  "Merge two header maps (second map takes precedence).
  
  Args:
    headers1: First headers map
    headers2: Second headers map (values override first)
    
  Returns:
    Merged headers map"
  ((headers1 headers2) (when (andalso (is_map headers1) (is_map headers2)))
   (maps:merge headers1 headers2)))

(defun filter
  "Filter headers by predicate function.
  
  Args:
    pred: Function that takes #(key value) and returns boolean
    headers: Headers map
    
  Returns:
    Filtered headers map"
  ((pred headers) (when (andalso (is_function pred 1) (is_map headers)))
   (maps:from_list
     (lists:filter pred (maps:to_list headers)))))

(defun remove
  "Remove a header by key.
  
  Args:
    headers: Headers map
    key: Header name to remove
    
  Returns:
    Headers map without the specified key"
  ((headers key)
   (maps:remove (normalize-key key) headers)))

;;; ---------------------------------------------------------------------------
;;; Utility Operations
;;; ---------------------------------------------------------------------------

(defun keys
  "Get list of all header keys.
  
  Args:
    headers: Headers map
    
  Returns:
    List of binary header keys"
  ((headers) (when (is_map headers))
   (maps:keys headers)))

(defun values
  "Get list of all header values.
  
  Args:
    headers: Headers map
    
  Returns:
    List of binary header values"
  ((headers) (when (is_map headers))
   (maps:values headers)))

;;; ---------------------------------------------------------------------------
;;; Private Helper Functions
;;; ---------------------------------------------------------------------------

(defun normalize-key
  "Normalize a header