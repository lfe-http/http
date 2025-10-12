(defmodule http.header
  (export
   ;; Legacy API (maintained for compatibility)
   (add 2) (add 3)
   (new 0)
   (list->map 1)

   ;; New API (renamed for clarity)
   (from-list 1)        ; Replaces list->map

   ;; Case-insensitive operations
   (get 2) (get 3) (get 4)
   (has-key? 2) (has-key? 3)

   ;; Bulk operations
   (merge 2)
   (filter 2)
   (remove 2)

   ;; Utility operations
   (to-list 1)
   (keys 1)
   (values 1)
   (normalize-key 1)))

;; NOTE: Inline directives would go here for hot-path functions
;; (normalize-key, normalize-kv, find-key)

;;; ---------------------------------------------------------------------------
;;; Core API
;;; ---------------------------------------------------------------------------

(defun new ()
  "Create a new empty headers map.

  Returns:
    Empty map ready for binary key-value pairs"
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

(defun list->map
  "Legacy API - convert property list to map.
  Calls from-list for backward compatibility.

  Args:
    proplist: List of tuples #(key value)

  Returns:
    Headers map"
  ((proplist)
   (from-list proplist)))

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
  ((headers key (= `#m(case-insensitive true) opts))
   (get headers key 'undefined opts))
  ((headers key default)
   (maps:get (normalize-key key) headers default)))

(defun get
  "Get header value with default (case-insensitive).

  Args:
    headers: Headers map
    key: Header name (any case)
    default: Default value if not found

  Returns:
    Header value or default"
  ((headers key default (= `#m(case-insensitive true) _opts))
   (let ((key-lower (http.util:binary-downcase (http.util:ensure-binary key))))
     (case (find-key headers key-lower)
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

(defun has-key?
  "Check if header exists (case-insensitive).

  Args:
    headers: Headers map
    key: Header name (any case)

  Returns:
    Boolean true/false"
  ((headers key (= `#m(case-insensitive true) _opts))
   (let ((key-lower (http.util:binary-downcase (http.util:ensure-binary key))))
     (case (find-key headers key-lower)
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
  "Normalize a header key to binary format.
  Single-pass conversion - no recursion.

  Args:
    key: Header key (binary, string, or atom)

  Returns:
    Binary header key"
  ((key) (when (is_binary key)) key)
  ((key) (when (is_list key)) (list_to_binary key))
  ((key) (when (is_atom key)) (atom_to_binary key)))

(defun normalize-kv
  "Normalize a key-value tuple to binary format.
  Single-pass conversion - no recursion.

  Args:
    kv: Tuple #(key value)

  Returns:
    Tuple #(binary-key binary-value)"
  ((`#(,k ,v))
   `#(,(http.util:ensure-binary k) ,(http.util:ensure-binary v))))

(defun find-key
  "Find a key in headers map or in list using case-insensitive search.

  Args:
    headers: Headers map
    key-lower: Lowercase binary key to search for

  Returns:
    Found key or 'undefined"
  (('() _) 'undefined)
  (((cons key rest) key-lower)
   (let ((key-lower-test (http.util:binary-downcase key)))
     (if (== key-lower-test key-lower)
       key
       (find-key rest key-lower))))
  ((headers key-lower)
   (let ((all-keys (maps:keys headers)))
     (find-key all-keys key-lower))))
