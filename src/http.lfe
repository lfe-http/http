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

(defun default-headers ()
  "Return default headers map for new requests.

  Returns:
    Map with User-Agent and Accept headers"
  #m(#"User-Agent" #"lfe-http/1.0.0"
     #"Accept" #"*/*"))

(defun default-version ()
  "Return default HTTP version.

  Returns:
    Float 1.1"
  1.1)

(defun methods ()
  "Return list of supported HTTP methods as atoms (legacy).

  Returns:
    List of method atoms"
  '(delete get head options patch post put trace connect))

(defun versions ()
  "Return list of supported HTTP versions.

  Returns:
    List of version numbers"
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
