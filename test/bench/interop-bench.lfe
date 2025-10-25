(defmodule interop-bench
  (export
   (run 0)
   (run-to-file 0)
   (benchmark-erlang-conversion-get 0)
   (benchmark-erlang-conversion-get 1)
   (benchmark-erlang-conversion-post 0)
   (benchmark-erlang-conversion-post 1)
   (benchmark-erlang-to-lfe 0)
   (benchmark-erlang-to-lfe 1)))

(defun run ()
  (io:format "~n=== HTTP Interop Benchmarks ===~n")
  (benchmark-erlang-conversion-get 'standard_io)
  (benchmark-erlang-conversion-post 'standard_io)
  (benchmark-erlang-to-lfe 'standard_io))

(defun run-to-file ()
  "Run benchmarks and write results to timestamped file in bench/"
  (case (util-bench:create-bench-file)
    (`#(ok ,device ,filename)
     (io:format device "=== HTTP Interop Benchmarks ===~n" '())
     (io:format device "Timestamp: ~s~n~n" (list (util-bench:format-timestamp)))
     (benchmark-erlang-conversion-get device)
     (benchmark-erlang-conversion-post device)
     (benchmark-erlang-to-lfe device)
     (file:close device)
     (io:format "Results written to ~s~n" (list filename)))
    (`#(error ,reason)
     (io:format "Error opening file: ~p~n" (list reason)))))

(defun benchmark-erlang-conversion-get ()
  (benchmark-erlang-conversion-get 'standard_io))

(defun benchmark-erlang-conversion-get (device)
  "Benchmark ->erlang conversion for GET requests"
  (let* ((req (http.request:new #"GET" "http://example.com"))
         (warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.c:->erlang req)))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.c:->erlang req))
                 (lists:seq 1 iterations))))))
      (io:format device "  ->erlang (GET): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))

(defun benchmark-erlang-conversion-post ()
  (benchmark-erlang-conversion-post 'standard_io))

(defun benchmark-erlang-conversion-post (device)
  "Benchmark ->erlang conversion for POST requests with body and headers"
  (let* ((req (http.request:new #"POST" "http://example.com/api"
                                #"body" #m(#"X-Custom" #"value")))
         (warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.c:->erlang req)))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.c:->erlang req))
                 (lists:seq 1 iterations))))))
      (io:format device "  ->erlang (POST): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))

(defun benchmark-erlang-to-lfe ()
  (benchmark-erlang-to-lfe 'standard_io))

(defun benchmark-erlang-to-lfe (device)
  "Benchmark erlang-> conversion from httpc response"
  (let* ((httpc-resp `#(#(#(1 1) 200 #"OK")
                        (#(#"content-type" "text/html")
                         #(#"content-length" "123"))
                        #"response body"))
         (warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.c:erlang-> httpc-resp)))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.c:erlang-> httpc-resp))
                 (lists:seq 1 iterations))))))
      (io:format device "  erlang->: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))
