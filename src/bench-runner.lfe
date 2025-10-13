(defmodule bench-runner
  (export
   (run 0)
   (run-to-file 0)))

(defun run ()
  "Run all HTTP library benchmarks to console"
  (io:format "~n=== Running All HTTP Library Benchmarks ===~n")
  (util-bench:run)
  (request-bench:run)
  (interop-bench:run)
  (io:format "~n=== All Benchmarks Complete ===~n"))

(defun run-to-file ()
  "Run all benchmarks and write results to a single timestamped file in bench/"
  (case (util-bench:create-bench-file)
    (`#(ok ,device ,filename)
     (io:format device "=== All HTTP Library Benchmarks ===~n" '())
     (io:format device "Timestamp: ~s~n~n" (list (util-bench:format-timestamp)))

     ;; Util benchmarks
     (io:format device "=== HTTP Util Benchmarks ===~n" '())
     (util-bench:bench-ensure-binary device)
     (util-bench:bench-binary-upcase device)
     (util-bench:bench-query-string device)

     ;; Request benchmarks
     (io:format device "~n=== HTTP Request Benchmarks ===~n" '())
     (request-bench:bench-construction device)
     (request-bench:bench-setters device)
     (request-bench:bench-helpers device)

     ;; Interop benchmarks
     (io:format device "~n=== HTTP Interop Benchmarks ===~n" '())
     (interop-bench:benchmark-erlang-conversion-get device)
     (interop-bench:benchmark-erlang-conversion-post device)
     (interop-bench:benchmark-erlang-to-lfe device)

     (file:close device)
     (io:format "~nAll benchmark results written to ~s~n" (list filename)))
    (`#(error ,reason)
     (io:format "Error opening file: ~p~n" (list reason)))))
