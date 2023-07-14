(defmodule http.tooling.logging
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun start ()
  (logger:set_application_level 'plottah 'all)
  (let ((cfg-file (++ (code:priv_dir 'logjam) "/config/dev.config")))
    (logjam:set-config `#(path ,cfg-file)))
  (log-info "Logging setup complete."))

(defun stop ()
  (log-info "Finishing script ...")
  (timer:sleep 1000))
