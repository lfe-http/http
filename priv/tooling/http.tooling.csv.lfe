(defmodule http.tooling.csv
  (export all))

(defun read-mimetypes ()
  (lists:map
   #'read-csv/1
   (http.tooling.files:mime-types)))

(defun read-header-fields ()
  #"")

(defun read-csv (file)
  (let* ((name (filename:basename file ".csv"))
         (`#(ok ,raw) (file:read_file file))
         (`#(ok (,raw-headers . ,csv)) (erl_csv:decode raw))
         (headers (trim raw-headers)))
    `#m(section ,name
        data ,(lists:map
               (lambda (row)
                 (maps:from_list
                  (lists:zip headers
                             (trim (lists:sublist row (length headers))))))
               csv))))

(defun trim (es)
  (lists:map
   (lambda (e)
     (list_to_binary (string:trim (binary_to_list e))))
   es))
