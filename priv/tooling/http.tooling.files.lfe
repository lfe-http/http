(defmodule http.tooling.files
  (export all))

(defun header-dir ()
  (++ (code:priv_dir 'http) "/header-fields"))

(defun mime-dir ()
  (++ (code:priv_dir 'http) "/mime-types"))
  
(defun header-fields ()
  (files (header-dir)))

(defun mime-types ()
  (files (mime-dir)))

(defun files (dir)
  (let ((`#(ok ,files) (file:list_dir dir)))
    (lists:map
     (lambda (filename)
       (++ dir "/" filename))
     files)))
