(defmodule http.tooling.code
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun mime-type-funcs (sections)
  (log-debug "Processing mime-type CSV sections ...")
  (let ((funcs (lists:map
                #'mime-type-funcs-section/1
                sections)))
    (io_lib:format "(defmodule http.mimetypes~n  (export all))~n~n~s"
                   (list (lists:join "\n" funcs)))))

(defun mime-type-funcs-section
  ((`#m(section ,section data ,rows))
   (log-debug "Processing mime-type CSV section '~s' ..." (list section))
   (let ((funcs (lists:map
                 (lambda (row)
                   (mime-type-func section row))
                 rows)))
   (io_lib:format ";;; ~s~n~n~s~n" (list section (lists:join "\n" funcs))))))

(defun mime-type-func
  ((section `#m(#"Name" ,name #"Template" #""))
   (mime-type-func (io_lib:format "~s/~s" (list section (binary_to_list name)))))
  ((_ `#m(#"Template" ,template))
   (mime-type-func (binary_to_list template))))

(defun mime-type-func (value)
  (log-debug "Creating function for '~s' ... " (list value))
  (io_lib:format "(defun ~s () \"~s\")" (list value value)))
