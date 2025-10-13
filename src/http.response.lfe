(defmodule http.response
  (export
   ;; Constructors
   (new 0) (new 1) (new 2) (new 3)

   ;; Setters
   (set-status 2)
   (set-body 2)
   (set-headers 2)
   (set-header 3)
   (add-header 3)
   (remove-header 2)

   ;; Getters
   (status 1)
   (body 1)
   (headers 1)

   ;; Convenience builders
   (ok 0) (ok 1)
   (created 0) (created 1)
   (accepted 0) (accepted 1)
   (no-content 0)
   (bad-request 0) (bad-request 1)
   (unauthorized 0) (unauthorized 1)
   (forbidden 0) (forbidden 1)
   (not-found 0) (not-found 1)
   (error 0) (error 1)
   (bad-gateway 0) (bad-gateway 1)
   (unavailable 0) (unavailable 1)

   ;; Content-type helpers
   (json 2)
   (text 2)
   (html 2)
   (xml 2)))

;;; ---------------------------------------------------------------------------
;;; Constructors
;;; ---------------------------------------------------------------------------

(defun new ()
  #m(status 200
     headers #m()
     body #""
     version 1.1))

(defun new
  ((status) (when (is_integer status))
   (mset (new) 'status status)))

(defun new (status body)
  (let* ((r (new))
         (r2 (mset r 'status status)))
    (mset r2 'body (ensure-binary body))))

(defun new (s h b)
  `#m(status ,s
      headers ,h
      body ,(ensure-binary b)
      version 1.1))

;;; ---------------------------------------------------------------------------
;;; Setters
;;; ---------------------------------------------------------------------------

(defun set-status
  "Set the HTTP status code.

  Args:
    resp: Response map
    status: HTTP status code (integer)

  Returns:
    Updated response map"
  ((resp status) (when (is_integer status))
   (mset resp 'status status)))

(defun set-body
  "Set the response body.

  Args:
    resp: Response map
    body: Body content (will be converted to binary)

  Returns:
    Updated response map"
  ((resp body)
   (mset resp 'body (ensure-binary body))))

(defun set-headers
  "Replace all headers.

  Args:
    resp: Response map
    headers: New headers map

  Returns:
    Updated response map"
  ((resp headers) (when (is_map headers))
   (mset resp 'headers headers)))

(defun set-header
  "Set a single header.

  Args:
    resp: Response map
    key: Header name
    val: Header value

  Returns:
    Updated response map"
  ((resp key val)
   (let* ((hs (mref resp 'headers))
          (hs2 (http.header:add hs key val)))
     (mset resp 'headers hs2))))

(defun add-header
  "Add a header (alias for set-header).

  Args:
    resp: Response map
    key: Header name
    val: Header value

  Returns:
    Updated response map"
  ((resp key val)
   (set-header resp key val)))

(defun remove-header
  "Remove a header.

  Args:
    resp: Response map
    key: Header name

  Returns:
    Updated response map"
  ((resp key)
   (let* ((hs (mref resp 'headers))
          (hs2 (http.header:remove hs key)))
     (mset resp 'headers hs2))))

;;; ---------------------------------------------------------------------------
;;; Getters
;;; ---------------------------------------------------------------------------

(defun status ((resp) (mref resp 'status)))
(defun body ((resp) (mref resp 'body)))
(defun headers ((resp) (mref resp 'headers)))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (2xx Success)
;;; ---------------------------------------------------------------------------

(defun ok () (new 200))
(defun ok (body) (new 200 body))

(defun created () (new 201))
(defun created (body) (new 201 body))

(defun accepted () (new 202))
(defun accepted (body) (new 202 body))

(defun no-content () (new 204))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (4xx Client Error)
;;; ---------------------------------------------------------------------------

(defun bad-request () (new 400))
(defun bad-request (body) (new 400 body))

(defun unauthorized () (new 401))
(defun unauthorized (body) (new 401 body))

(defun forbidden () (new 403))
(defun forbidden (body) (new 403 body))

(defun not-found () (new 404))
(defun not-found (body) (new 404 body))

;;; ---------------------------------------------------------------------------
;;; Convenience Builders (5xx Server Error)
;;; ---------------------------------------------------------------------------

(defun error () (new 500))
(defun error (body) (new 500 body))

(defun bad-gateway () (new 502))
(defun bad-gateway (body) (new 502 body))

(defun unavailable () (new 503))
(defun unavailable (body) (new 503 body))

;;; ---------------------------------------------------------------------------
;;; Content-Type Helpers
;;; ---------------------------------------------------------------------------

(defun json
  "Create JSON response with status and data.

  Args:
    status: HTTP status code
    json-body: JSON body (already encoded as binary)

  Returns:
    Response with JSON content-type"
  ((status json-body)
   (set-header (new status json-body)
               #"Content-Type"
               #"application/json; charset=utf-8")))

(defun text
  "Create plain text response.

  Args:
    status: HTTP status code
    text-body: Text body

  Returns:
    Response with text content-type"
  ((status text-body)
   (set-header (new status text-body)
               #"Content-Type"
               #"text/plain; charset=utf-8")))

(defun html
  "Create HTML response.

  Args:
    status: HTTP status code
    html-body: HTML body

  Returns:
    Response with HTML content-type"
  ((status html-body)
   (set-header (new status html-body)
               #"Content-Type"
               #"text/html; charset=utf-8")))

(defun xml
  "Create XML response.

  Args:
    status: HTTP status code
    xml-body: XML body

  Returns:
    Response with XML content-type"
  ((status xml-body)
   (set-header (new status xml-body)
               #"Content-Type"
               #"application/xml; charset=utf-8")))

;;; ---------------------------------------------------------------------------
;;; Private Helpers
;;; ---------------------------------------------------------------------------

(defun ensure-binary
  "Convert value to binary (inline-optimized).

  Args:
    val: Value to convert

  Returns:
    Binary representation"
  ((b) (when (is_binary b)) b)
  ((l) (when (is_list l)) (iolist_to_binary l))
  ((a) (when (is_atom a)) (atom_to_binary a))
  ((i) (when (is_integer i)) (integer_to_binary i)))
