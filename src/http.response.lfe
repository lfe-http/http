(defmodule http.response
  (export
   (new 0) (new 1) (new 2) (new 3)
   (erlang-> 1)))

(defun new ()
  (new #""))

(defun new (body)
  (new (http.status:ok) body))

(defun new (status body)
  (new status '() body))

(defun new (status headers body)
  `#m(status ,status
      headers ,headers
      body ,body))

(defun erlang-> (httpc-resp)
  "Convert an Erlang HTTP client (httpc) response to an LFE HTTP library
  response."
  (let ((`#(,_ ,status-code ,_) (element 1 httpc-resp))
        (headers (http.header:list->map (element 2 httpc-resp)))
        (body (list_to_binary (element 3 httpc-resp))))
    (new status-code headers body)))
