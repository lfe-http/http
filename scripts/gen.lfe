#! /usr/bin/env lfescript

(include-lib "logjam/include/logjam.hrl")

(defun show-help ()
  "Usage:

  rebar3 as script lfe run -- <gen-target>

Description:

  This script is responsible for generating LFE files with various HTTP
  constants (functions, in LFE) defined and ready for use by this `http`
  library.

Targets:

  The following generation targets (arguments) are accepted:

  list-targets - this will output the list of supported generation targets.

  all - this will generate all supported target files.

  header-fields - this will generate HTTP header field constants and save
                  them to a temp file. The functions should be copied and
                  then pasted into the ./src/http.header.lfe file (replacing
                  old ones, if they exist).

  mime-types - this will generate HTTP mime-type constants and overwrite the
               existing file ./src/http.mimetypes.lfe with updated content.")
  
(defun gen-targets ()
  '(header-fields
    mime-types))

(defun gen
  (('list-targets)
   (print (gen-targets)
          "Supported gen targets:\n\n"))
  (('list-header-files)
   (print (http.tooling.files:header-fields)
          "Header-fields files:\n\n"))
  (('list-mime-files)
   (print (http.tooling.files:mime-types)
          "Mime-type files:\n\n"))
  (('all)
   (lists:map
    (lambda (name)
      (gen name)
      (gen-targets))))
  (('header-fields)
   (http.tooling:write-temp-header-fields))
  (('mime-types)
   (http.tooling:update-mimetypes-module)))

(defun main
  (('())
   (print-str (show-help)))
  ((`(,name . ,_))
   (http.tooling.logging:start)
   (gen (list_to_atom (binary_to_list name)))
   (http.tooling.logging:stop)
   (newline)))

;;; private functions

(defun newline ()
  (lfe_io:format "~n" '()))

(defun print-str (data)
  (print-str data ""))

(defun print-str (data prefix)
  (lfe_io:format "~n~s~s~n~n" `(,prefix ,data)))

(defun print (data)
  (print data ""))

(defun print (data prefix)
  (lfe_io:format "~n~s~p~n~n" `(,prefix ,data)))
