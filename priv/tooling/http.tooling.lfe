(defmodule http.tooling
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun update-mimetypes-module ()
  (log-info "Updating mime-types module ...")
  (let* ((filename "src/http.mimetype.lfe")
         (parsed (http.tooling.csv:read-mimetypes))
         (code (http.tooling.code:mime-type-funcs parsed))
         ('ok (file:write_file filename (list_to_binary code))))
    'ok))

(defun write-temp-header-fields ()
  (log-info "(Re)creating header-fields temp file ...")
  (let* ((filename "temp-file-header-fields.lfe")
         (data (http.tooling.csv:read-header-fields))
         ('ok (file:write_file filename data)))))
