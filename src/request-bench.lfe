(defmodule request-bench
  (export
   (run 0)
   (run-to-file 0)
   (bench-construction 0)
   (bench-construction 1)
   (bench-setters 0)
   (bench-setters 1)
   (bench-helpers 0)
   (bench-helpers 1)))

(defun run ()
  "Run benchmarks and display to console"
  (io:format "~n=== HTTP Request Benchmarks ===~n")
  (bench-construction 'standard_io)
  (bench-setters 'standard_io)
  (bench-helpers 'standard_io))

(defun run-to-file ()
  "Run benchmarks and write results to timestamped file in bench/"
  (case (util-bench:create-bench-file)
    (`#(ok ,device ,filename)
     (io:format device "=== HTTP Request Benchmarks ===~n" '())
     (io:format device "Timestamp: ~s~n~n" (list (util-bench:format-timestamp)))
     (bench-construction device)
     (bench-setters device)
     (bench-helpers device)
     (file:close device)
     (io:format "Results written to ~s~n" (list filename)))
    (`#(error ,reason)
     (io:format "Error opening file: ~p~n" (list reason)))))

(defun bench-construction ()
  (bench-construction 'standard_io))

(defun bench-construction (device)
  (io:format device "~nRequest construction performance:~n" '())

  ;; Simple URL only
  (let* ((warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:new "http://example.com")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_)
                   (http.request:new "http://example.com"))
                 (lists:seq 1 iterations))))))
      (io:format device "  new/1 (URL only): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations)))))

  ;; With method
  (let* ((warmup-iters 5000)
         (iterations 10000))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:new #"POST" "http://example.com")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_)
                   (http.request:new #"POST" "http://example.com"))
                 (lists:seq 1 iterations))))))
      (io:format device "  new/2 (method + URL): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations)))))

  ;; Full construction
  (let* ((warmup-iters 5000)
         (iterations 10000)
         (headers #m(#"X-Custom" #"value")))
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:new #"POST" "http://example.com" #"body" headers)))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_)
                   (http.request:new #"POST" "http://example.com" #"body" headers))
                 (lists:seq 1 iterations))))))
      (io:format device "  new/4 (full): ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))

(defun bench-setters ()
  (bench-setters 'standard_io))

(defun bench-setters (device)
  (io:format device "~nSetter operations performance:~n" '())

  (let* ((req (http.request:new "http://example.com"))
         (warmup-iters 10000)
         (iterations 100000))

    ;; set-method
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:set-method req #"POST")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-method req #"POST"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-method: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-body
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:set-body req #"body")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-body req #"body"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-body: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-header
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:set-header req #"X-Custom" #"value")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-header req #"X-Custom" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-header: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-helpers ()
  (bench-helpers 'standard_io))

(defun bench-helpers (device)
  (io:format device "~nHelper functions performance:~n" '())

  (let* ((req (http.request:new "http://example.com"))
         (json #"{\"key\":\"value\"}")
         (warmup-iters 5000)
         (iterations 10000))

    ;; set-json
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:set-json req json)))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-json req json))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-json: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))

    ;; add-query-param
    ;; Warm-up phase
    (util-bench:warmup warmup-iters (lambda () (http.request:add-query-param req #"key" #"value")))
    ;; Actual benchmark
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:add-query-param req #"key" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format device "  add-query-param: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))
