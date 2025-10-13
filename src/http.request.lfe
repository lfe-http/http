(defmodule http.request
  (export
   ;; Constructors
   (new 1) (new 2) (new 3) (new 4)

   ;; Setters (builder pattern)
   (set-method 2)
   (set-body 2)
   (set-headers 2)
   (set-header 3)
   (add-header 3)
   (remove-header 2)

   ;; Getters
   (method 1)
   (url 1)
   (body 1)
   (headers 1)
   (path-segments 1)
   (query-params 1)

   ;; Content-type helpers
   (set-json 2)
   (set-form 2)
   (set-text 2)

   ;; Query parameter helpers
   (add-query-param 3)
   (set-query-params 2)

   ;; Debugging (optional, for development)
   (to-map 1)))

;;; ---------------------------------------------------------------------------
;;; Constructors (Optimized for Single-Pass Construction)
;;; ---------------------------------------------------------------------------

(defun new
  "Create a new HTTP request with URL.
  Single-pass construction - all parsing done once.

  Args:
    url: Request URL (binary or string)

  Returns:
    Request map with GET method and empty headers"
  ((url) (when (is_list url))
   (new (list_to_binary url)))
  ((url) (when (is_binary url))
   (let* ((parsed (yuri:parse url))
          (parsed-full (maps:merge parsed (yuri.user:parse parsed)))
          (path (maps:get 'path parsed-full #""))
          (query (maps:get 'query parsed-full #""))
          (path-segs (yuri.path:->segments path))
          (query-pars (yuri.query:parse query)))
     ;; Direct map construction - no merges
     `#m(method #"GET"
         version 1.1
         remote-addr #""
         headers #m()
         body #""
         url ,url
         url-parsed ,parsed-full
         path-segments ,path-segs
         query-parsed ,query-pars))))

(defun new
  "Create request with method and URL.

  Args:
    method: HTTP method (binary, atom, or string)
    url: Request URL

  Returns:
    Request map"
  ((method url)
   (let ((req (new url)))
     (mset req 'method (ensure-method-binary method)))))

(defun new
  "Create request with method, URL, and body.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body (binary, string, or iolist)

  Returns:
    Request map"
  ((method url body)
   (let ((req (new method url)))
     (mset req 'body (http.util:ensure-binary body)))))

(defun new
  "Create request with method, URL, body, and headers.

  Args:
    method: HTTP method
    url: Request URL
    body: Request body
    headers: Headers map

  Returns:
    Request map"
  ((method url body headers)
   (let* ((req (new method url body))
          (req2 (mset req 'headers headers)))
     req2)))

;;; ---------------------------------------------------------------------------
;;; Setters (Builder Pattern)
;;; ---------------------------------------------------------------------------

(defun set-method
  "Set the HTTP method.

  Args:
    req: Request map
    method: HTTP method (will be converted to uppercase binary)

  Returns:
    Updated request map"
  ((req method)
   (mset req 'method (ensure-method-binary method))))

(defun set-body
  "Set the request body.

  Args:
    req: Request map
    body: Body content (binary, string, iolist, atom, or integer)

  Returns:
    Updated request map"
  ((req body)
   (mset req 'body (http.util:ensure-binary body))))

(defun set-headers
  "Replace all headers.

  Args:
    req: Request map
    headers: New headers map

  Returns:
    Updated request map"
  ((req headers) (when (is_map headers))
   (mset req 'headers headers)))

(defun set-header
  "Set a single header (replaces if exists).

  Args:
    req: Request map
    key: Header name
    val: Header value

  Returns:
    Updated request map"
  ((req key val)
   (let* ((hs (mref req 'headers))
          (hs2 (http.header:add hs key val)))
     (mset req 'headers hs2))))

(defun add-header
  "Add a header (alias for set-header for compatibility).

  Args:
    req: Request map
    key: Header name
    val: Header value

  Returns:
    Updated request map"
  ((req key val)
   (set-header req key val)))

(defun remove-header
  "Remove a header.

  Args:
    req: Request map
    key: Header name to remove

  Returns:
    Updated request map"
  ((req key)
   (let* ((hs (mref req 'headers))
          (hs2 (http.header:remove hs key)))
     (mset req 'headers hs2))))

;;; ---------------------------------------------------------------------------
;;; Getters (Inline-Optimized)
;;; ---------------------------------------------------------------------------

(defun method
  "Get HTTP method from request.

  Args:
    req: Request map

  Returns:
    Binary HTTP method"
  ((req) (mref req 'method)))

(defun url
  "Get URL from request.

  Args:
    req: Request map

  Returns:
    Binary URL"
  ((req) (mref req 'url)))

(defun body
  "Get body from request.

  Args:
    req: Request map

  Returns:
    Binary body"
  ((req) (mref req 'body)))

(defun headers
  "Get headers from request.

  Args:
    req: Request map

  Returns:
    Headers map"
  ((req) (mref req 'headers)))

(defun path-segments
  "Get URL path segments.

  Args:
    req: Request map

  Returns:
    List of binary path segments"
  ((req) (mref req 'path-segments)))

(defun query-params
  "Get query parameters.

  Args:
    req: Request map

  Returns:
    Map of query parameters"
  ((req) (mref req 'query-parsed)))

;;; ---------------------------------------------------------------------------
;;; Content-Type Helpers
;;; ---------------------------------------------------------------------------

(defun set-json
  "Set body and Content-Type for JSON.

  Args:
    req: Request map
    json-body: JSON body (already encoded as binary)

  Returns:
    Updated request map with JSON content-type"
  ((req json-body)
   (let* ((req2 (set-body req json-body))
          (req3 (set-header req2 #"Content-Type" #"application/json; charset=utf-8")))
     req3)))

(defun set-form
  "Set body and Content-Type for form data.

  Args:
    req: Request map
    form-data: URL-encoded form data (binary) or params map

  Returns:
    Updated request map with form content-type"
  ((req form-data) (when (is_map form-data))
   (set-form req (http.util:query-string form-data)))
  ((req form-data) (when (is_binary form-data))
   (let* ((req2 (set-body req form-data))
          (req3 (set-header req2 #"Content-Type" #"application/x-www-form-urlencoded")))
     req3)))

(defun set-text
  "Set body and Content-Type for plain text.

  Args:
    req: Request map
    text-body: Text body (binary or string)

  Returns:
    Updated request map with text content-type"
  ((req text-body)
   (let* ((req2 (set-body req text-body))
          (req3 (set-header req2 #"Content-Type" #"text/plain; charset=utf-8")))
     req3)))

;;; ---------------------------------------------------------------------------
;;; Query Parameter Helpers
;;; ---------------------------------------------------------------------------

(defun add-query-param
  "Add a query parameter to the request.
  Modifies both query-parsed map and reconstructs URL.

  Args:
    req: Request map
    key: Parameter name (will be converted to binary)
    val: Parameter value (will be converted to binary)

  Returns:
    Updated request map with new query parameter"
  ((req key val)
   (let* ((query-map (mref req 'query-parsed))
          (key-bin (http.util:ensure-binary key))
          (val-bin (http.util:ensure-binary val))
          (updated-query (maps:put key-bin val-bin query-map))
          (url-parsed (mref req 'url-parsed))
          (new-query-string (http.util:query-string updated-query))
          ;; Reconstruct URL with new query
          (base-url (binary-without-query (mref req 'url)))
          (new-url (if (== new-query-string #"")
                     base-url
                     (iolist_to_binary (list base-url #"?" new-query-string))))
          (req2 (mset req 'query-parsed updated-query))
          (req3 (mset req2 'url new-url))
          (req4 (mset req3 'url-parsed (maps:put 'query new-query-string url-parsed))))
     req4)))

(defun set-query-params
  "Replace all query parameters.

  Args:
    req: Request map
    params: Map of query parameters

  Returns:
    Updated request map"
  ((req params) (when (is_map params))
   (let* ((query-string (http.util:query-string params))
          (base-url (binary-without-query (mref req 'url)))
          (new-url (if (== query-string #"")
                     base-url
                     (iolist_to_binary (list base-url #"?" query-string))))
          (url-parsed (mref req 'url-parsed))
          (req2 (mset req 'query-parsed params))
          (req3 (mset req2 'url new-url))
          (req4 (mset req3 'url-parsed (maps:put 'query query-string url-parsed))))
     req4)))

;;; ---------------------------------------------------------------------------
;;; Private Helper Functions
;;; ---------------------------------------------------------------------------

(defun ensure-method-binary
  "Convert method to uppercase binary.
  Inline-optimized for hot-path usage.

  Args:
    method: HTTP method (binary, atom, or string)

  Returns:
    Uppercase binary method"
  ((method) (when (is_binary method))
   (http.util:binary-upcase method))
  ((method) (when (is_atom method))
   (http.util:binary-upcase (atom_to_binary method)))
  ((method) (when (is_list method))
   (http.util:binary-upcase (list_to_binary method))))

(defun binary-without-query
  "Remove query string from URL.

  Args:
    url: Binary URL

  Returns:
    URL without query string"
  ((url) (when (is_binary url))
   (case (binary:split url #"?")
     (`(,base ,_) base)
     (`(,base) base))))

;;; ---------------------------------------------------------------------------
;;; Debugging Utilities
;;; ---------------------------------------------------------------------------

(defun to-map
  "Convert request to sorted map representation for debugging.
  This is only for development/debugging - not optimized.

  Args:
    req: Request map

  Returns:
    Request map with sorted nested structures"
  ((req) (when (is_map req))
   (let* ((h (maps:get 'headers req #m()))
          (q (maps:get 'query-parsed req #m()))
          (u (maps:get 'url-parsed req #m())))
     (maps:merge req
                 `#m(headers ,(maps:from_list (lists:sort (maps:to_list h)))
                     query-parsed ,(maps:from_list (lists:sort (maps:to_list q)))
                     url-parsed ,(maps:from_list (lists:sort (maps:to_list u))))))))
