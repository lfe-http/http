#!/usr/bin/env lfe

;;;; Automated migration helper for v0.5.4 -> v1.0.0
;;;; Usage: ./MIGRATION_SCRIPT.lfe <directory>

(defmodule migration-script
  (export (main 1)))

(defun main
  (([dir])
   (io:format "LFE HTTP Library Migration Script~n")
   (io:format "Migrating files in: ~s~n~n" (list dir))

   (let ((files (find-lfe-files dir)))
     (io:format "Found ~p LFE files~n" (list (length files)))
     (lists:foreach #'migrate-file/1 files)
     (io:format "~nMigration complete!~n")
     (io:format "Please review changes and run tests.~n")))
  ((_)
   (io:format "Usage: ./MIGRATION_SCRIPT.lfe <directory>~n")
   (erlang:halt 1)))

(defun find-lfe-files (dir)
  (filelib:fold_files dir ".*.lfe$" 'true #'cons/2 '()))

(defun migrate-file (filepath)
  (io:format "Migrating: ~s~n" (list filepath))
  (case (file:read_file filepath)
    (`#(ok ,content)
     (let* ((content-str (binary_to_list content))
            (migrated (migrate-content content-str))
            (backup (++ filepath ".bak")))
       ;; Create backup
       (file:write_file backup content)
       ;; Write migrated content
       (file:write_file filepath (list_to_binary migrated))
       (io:format "  ✓ Migrated (backup: ~s)~n" (list backup))))
    (`#(error ,reason)
     (io:format "  ✗ Error: ~p~n" (list reason)))))

(defun migrate-content (content)
  (-> content
      (migrate-methods)
      (migrate-header-functions)))

(defun migrate-methods (content)
  ;; Replace atom methods with binary methods
  (let ((replacements
         '(("'get" . "#\"GET\"")
           ("'post" . "#\"POST\"")
           ("'put" . "#\"PUT\"")
           ("'delete" . "#\"DELETE\"")
           ("'patch" . "#\"PATCH\"")
           ("'head" . "#\"HEAD\"")
           ("'options" . "#\"OPTIONS\"")
           ("'trace" . "#\"TRACE\""))))
    (lists:foldl
      (lambda (replacement acc)
        (let ((`#(,old ,new) replacement))
          (re:replace acc old new '(global #(return list)))))
      content
      replacements)))

(defun migrate-header-functions (content)
  ;; Replace list->map with from-list
  (re:replace content
              "http.header:list->map"
              "http.header:from-list"
              '(global #(return list))))
