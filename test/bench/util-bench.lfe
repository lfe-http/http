(defmodule util-bench
  (export
   (run 0)
   (run-to-file 0)
   (bench-ensure-binary 0)
   (bench-ensure-binary 1)
   (bench-binary-upcase 0)
   (bench-binary-upcase 1)
   (bench-query-string 0)
   (bench-query-string 1)
   ;; Shared benchmark utilities
   (warmup 2)
   (create-bench-file 0)
   (format-timestamp 0)))

(defun run ()
  (io:format "~n=== HTTP Util Benchmarks ===~n")
  (bench-ensure-binary 'standard_io)
  (bench-binary-upcase 'standard_io)
  (bench-query-string 'standard_io))

(defun run-to-file ()
  "Run benchmarks and write results to timestamped file in bench/"
  (case (create-bench-file)
    (`#(ok ,device ,filename)
     (io:format device "=== HTTP Util Benchmarks ===~n" '())
     (io:format device "Timestamp: ~s~n~n" (list (format-timestamp)))
     (bench-ensure-binary device)
     (bench-binary-upcase device)
     (bench-query-string device)
     (file:close device)
     (io:format "Results written to ~s~n" (list filename)))
    (`#(error ,reason)
     (io:format "Error opening file: ~p~n" (list reason)))))

(defun bench-ensure-binary ()
  (bench-ensure-binary 'standard_io))

(defun bench-ensure-binary (device)
  (io:format device "~nensure-binary/1 performance:~n" '())

  ;; Binary input (should be instant - zero-copy)
  (let* ((warmup-iters 100000)
         (iterations 1000000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:ensure-binary #"test")))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:ensure-binary #"test"))
                 (lists:seq 1 iterations))))))
      (io:format device "  Binary input: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

  ;; Atom input (conversion required)
  (let* ((warmup-iters 10000)
         (iterations 100000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:ensure-binary 'test)))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:ensure-binary 'test))
                 (lists:seq 1 iterations))))))
      (io:format device "  Atom input: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

  ;; List input (conversion required)
  (let* ((warmup-iters 10000)
         (iterations 100000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:ensure-binary "test")))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:ensure-binary "test"))
                 (lists:seq 1 iterations))))))
      (io:format device "  List input: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-binary-upcase ()
  (bench-binary-upcase 'standard_io))

(defun bench-binary-upcase (device)
  (io:format device "~nbinary-upcase/1 performance:~n" '())

  (let* ((warmup-iters 10000)
         (iterations 100000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:binary-upcase #"content-type")))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:binary-upcase #"content-type"))
                 (lists:seq 1 iterations))))))
      (io:format device "  ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-query-string ()
  (bench-query-string 'standard_io))

(defun bench-query-string (device)
  (io:format device "~nquery-string/1 performance:~n" '())

  ;; Small map (3 params)
  (let* ((params #m(#"key1" #"value1"
                    #"key2" #"value2"
                    #"key3" #"value3"))
         (warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:query-string params)))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:query-string params))
                 (lists:seq 1 iterations))))))
      (io:format device "  Small map (3 params): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations)))))

  ;; Larger map (10 params)
  (let* ((params #m(#"key1" #"value1"
                    #"key2" #"value2"
                    #"key3" #"value3"
                    #"key4" #"value4"
                    #"key5" #"value5"
                    #"key6" #"value6"
                    #"key7" #"value7"
                    #"key8" #"value8"
                    #"key9" #"value9"
                    #"key10" #"value10"))
         (warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up
    (warmup warmup-iters (lambda () (http.util:query-string params)))
    ;; Benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.util:query-string params))
                 (lists:seq 1 iterations))))))
      (io:format device "  Larger map (10 params): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))

;;; ---------------------------------------------------------------------------
;;; Shared Benchmark Utilities
;;; ---------------------------------------------------------------------------

(defun warmup (iterations operation)
  "Run warmup iterations for a benchmark operation.

  Args:
    iterations: Number of warmup iterations
    operation: Zero-arity lambda to execute

  Returns:
    'ok"
  (lists:foreach (lambda (_) (funcall operation)) (lists:seq 1 iterations))
  'ok)

(defun create-bench-file ()
  "Create a timestamped benchmark results file.

  Returns:
    #(ok device filename) or #(error reason)"
  (let* ((timestamp (erlang:system_time 'second))
         (filename (++ "bench/benchmark-results-"
                       (integer_to_list timestamp)
                       ".txt")))
    (case (file:open filename '(write))
      (`#(ok ,device)
       `#(ok ,device ,filename))
      (`#(error ,reason)
       `#(error ,reason)))))

(defun format-timestamp ()
  "Format current timestamp as ISO 8601 string.

  Returns:
    String representation of current time"
  (let* ((now (calendar:universal_time))
         (`#(#(,year ,month ,day) #(,hour ,min ,sec)) now))
    (io_lib:format "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC"
                   (list year month day hour min sec))))
