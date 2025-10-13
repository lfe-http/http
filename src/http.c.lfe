;;;; This module provides interoperability between the LFE HTTP library and the
;;;; Erlang stdlib httpc HTTP client library.
(defmodule http.c
  (export
   (->erlang 1) (->erlang 3)
   (erlang-> 1)
   (request 1) (request 2) (request 3) (request 4) (request 5) (request 6)))

;;; ---------------------------------------------------------------------------
;;; Public Request Functions
;;; ---------------------------------------------------------------------------

(defun request
  "Make HTTP request with URL only (GET request).

  Args:
    url: Request URL (string or binary)

  Returns:
    #(ok response) or error tuple"
  ((url)
   (request-internal (http.request:new url))))

(defun request
  "Make HTTP request with method and URL.

  Args:
    method: HTTP method (binary, atom, or string)
    url: Request URL

  Returns:
    #(ok response) or error tuple"
  ((method url)
   (request-internal (http.request:new method url))))

(defun request
  "Make HTTP request with method, URL, and body.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body

  Returns:
    #(ok response) or error tuple"
  ((method url body)
   (request-internal (http.request:new method url body))))

(defun request
  "Make HTTP request with method, URL, body, and headers.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map

  Returns:
    #(ok response) or error tuple"
  ((method url body headers)
   (request-internal (http.request:new method url body headers))))

(defun request
  "Make HTTP request with HTTP options.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options

  Returns:
    #(ok response) or error tuple"
  ((method url body http-options options)
   (let ((req (http.request:new method url body)))
     (request-internal req http-options options))))

(defun request
  "Make HTTP request with headers and HTTP options.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options

  Returns:
    #(ok response) or error tuple"
  ((method url body headers http-options options)
   (let ((req (http.request:new method url body headers)))
     (request-internal req http-options options))))

;;; ---------------------------------------------------------------------------
;;; Internal Request Functions
;;; ---------------------------------------------------------------------------

(defun request-internal
  "Internal request with default options.

  Args:
    req: LFE HTTP request map

  Returns:
    #(ok response) or error tuple"
  ((req)
   (request-internal req '() '())))

(defun request-internal
  "Internal request with HTTP options.

  Args:
    req: LFE HTTP request map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options

  Returns:
    #(ok response) or error tuple"
  ((req http-options options)
   (let* ((version (mref req 'version))
          (version-tuple (http.util:http-version-tuple version))
          ;; Merge version into http-options
          (http-opts (cons `#(version ,version-tuple) http-options))
          ;; Ensure sync and full_result
          (opts (lists:append options '(#(sync true) #(full_result true))))
          ;; Convert to Erlang format
          (args (->erlang req http-opts opts)))
     (case (apply #'httpc:request/4 args)
       (`#(ok ,httpc-resp) `#(ok ,(erlang-> httpc-resp)))
       (err err)))))

;;; ---------------------------------------------------------------------------
;;; LFE -> Erlang Conversion
;;; ---------------------------------------------------------------------------

(defun ->erlang
  "Convert LFE HTTP request to Erlang httpc format with default options.

  Args:
    req: LFE HTTP request map

  Returns:
    List of arguments for httpc:request/4"
  ((req)
   (->erlang req
             `(#(version ,(http.util:http-version-tuple (mref req 'version))))
             '(#(sync true) #(full_result true)))))

(defun ->erlang
  "Convert LFE HTTP request to Erlang httpc format.

  Args:
    req: LFE HTTP request map
    http-options: Erlang httpc HTTP options
    options: Erlang httpc options

  Returns:
    List of arguments for httpc:request/4"
  ((req http-options options)
   (->erlang-dispatch (mref req 'method) req http-options options)))

;;; ---------------------------------------------------------------------------
;;; Binary Method Dispatch (Optimized)
;;; ---------------------------------------------------------------------------

(defun ->erlang-dispatch
  "Dispatch on binary HTTP method for optimized conversion.
  Uses binary pattern matching instead of atom comparison.

  Args:
    method: Binary HTTP method (e.g., #\"GET\")
    req: Request map
    http-options: HTTP options
    options: Options

  Returns:
    List of arguments for httpc:request/4"
  ;; Methods without body
  ((#"GET" req http-opts opts)
   (->erlang-no-body 'get req http-opts opts))
  ((#"HEAD" req http-opts opts)
   (->erlang-no-body 'head req http-opts opts))
  ((#"DELETE" req http-opts opts)
   (->erlang-no-body 'delete req http-opts opts))
  ((#"OPTIONS" req http-opts opts)
   (->erlang-no-body 'options req http-opts opts))
  ((#"TRACE" req http-opts opts)
   (->erlang-no-body 'trace req http-opts opts))

  ;; Methods with body
  ((#"POST" req http-opts opts)
   (->erlang-with-body 'post req http-opts opts))
  ((#"PUT" req http-opts opts)
   (->erlang-with-body 'put req http-opts opts))
  ((#"PATCH" req http-opts opts)
   (->erlang-with-body 'patch req http-opts opts))

  ;; Fallback for custom methods
  ((method req http-opts opts)
   (let ((method-atom (method->atom method)))
     (if (http:method-has-body? method)
       (->erlang-with-body method-atom req http-opts opts)
       (->erlang-no-body method-atom req http-opts opts)))))

;;; ---------------------------------------------------------------------------
;;; Conversion Helpers
;;; ---------------------------------------------------------------------------

(defun ->erlang-no-body
  "Convert request without body to Erlang format.
  Optimized for single-pass conversion.

  Args:
    method-atom: Erlang method atom
    req: Request map
    http-options: HTTP options
    options: Options

  Returns:
    List #(method url headers) http-options options"
  ((method-atom req http-opts opts)
   (let* ((url (mref req 'url))
          (headers (mref req 'headers))
          (header-list (maps:to_list headers)))
     `(,method-atom
       #(,url ,header-list)
       ,http-opts
       ,opts))))

(defun ->erlang-with-body
  "Convert request with body to Erlang format.
  Optimized for single-pass conversion.

  Args:
    method-atom: Erlang method atom
    req: Request map
    http-options: HTTP options
    options: Options

  Returns:
    List #(method url headers content-type body) http-options options"
  ((method-atom req http-opts opts)
   (let* ((url (mref req 'url))
          (headers (mref req 'headers))
          (header-list (maps:to_list headers))
          (body (mref req 'body))
          (content-type
           (http.header:get headers #"Content-Type"
                            #"application/octet-stream"
                            #m(case-insensitive true))))
     `(,method-atom
       #(,url
         ,header-list
         ,(binary_to_list content-type)
         ,body)
       ,http-opts
       ,(cons #(body_format binary) opts)))))

(defun method->atom
  "Convert binary method to lowercase atom for Erlang httpc.
  Inline-optimized.

  Args:
    method: Binary HTTP method

  Returns:
    Lowercase atom"
  ((method) (when (is_binary method))
   (http.util:binary-downcase-atom method)))

;;; ---------------------------------------------------------------------------
;;; Erlang -> LFE Conversion
;;; ---------------------------------------------------------------------------

(defun erlang->
  "Convert Erlang httpc response to LFE HTTP response.
  Single-pass conversion with optimized header handling.

  Args:
    httpc-resp: Erlang httpc response tuple

  Returns:
    LFE HTTP response map"
  ((httpc-resp)
   (let ((`#(#(,_version ,sc ,_reason-phrase) ,header-list ,body-data)
          httpc-resp))
     ;; Direct map construction - single pass
     `#m(status ,sc
         headers ,(http.header:from-list header-list)
         body ,(iolist_to_binary body-data)
         version 1.1))))
