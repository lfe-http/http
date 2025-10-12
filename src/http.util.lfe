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

;; NOTE: Inline directives would go here for hot-path functions
;; (ensure-binary, binary-upcase, binary-downcase)
;; TODO: Add inline optimization in future once LFE syntax is confirmed

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
  (('()) #"/")
  ((segments) (when (is_list segments))
   (iolist_to_binary
     (list #"/" (lists:join #"/" segments)))))

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
           (if (or (andalso (>= c 48) (=< c 57))   ; 0-9
                   (andalso (>= c 65) (=< c 90))   ; A-Z
                   (andalso (>= c 97) (=< c 122))  ; a-z
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
