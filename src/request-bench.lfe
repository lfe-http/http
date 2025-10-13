(defmodule request-bench
  (export
   (run 0)
   (run-to-file 0)
   (bench-construction 0)
   (bench-setters 0)
   (bench-helpers 0)))

(defun run ()
  "Run benchmarks and display to console"
  (io:format "~n=== HTTP Request Benchmarks ===~n")
  (bench-construction)
  (bench-setters)
  (bench-helpers))

(defun run-to-file ()
  "Run benchmarks and write results to ./bench/request-bench-results.txt"
  (let* ((timestamp (erlang:system_time 'second))
         (filename (++ "bench/request-bench-results-"
                       (integer_to_list timestamp)
                       ".txt"))
         (result (file:open filename '(write))))
    (case result
      (`#(ok ,device)
       (io:format device "=== HTTP Request Benchmarks ===~n" '())
       (io:format device "Timestamp: ~s~n~n" (list (format-timestamp)))
       (bench-construction-to-device device)
       (bench-setters-to-device device)
       (bench-helpers-to-device device)
       (file:close device)
       (io:format "Results written to ~s~n" (list filename)))
      (`#(error ,reason)
       (io:format "Error opening file: ~p~n" (list reason))))))

(defun bench-construction ()
  (io:format "~nRequest construction performance:~n")

  ;; Simple URL only
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format "  new/1 (URL only): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; With method
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format "  new/2 (method + URL): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Full construction
  (let* ((iterations 10000)
         (headers #m(#"X-Custom" #"value"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com" #"body" headers))
                (lists:seq 1 iterations))))))
    (io:format "  new/4 (full): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun bench-setters ()
  (io:format "~nSetter operations performance:~n")

  (let* ((req (http.request:new "http://example.com"))
         (iterations 100000))

    ;; set-method
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-method req #"POST"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-method: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-body
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-body req #"body"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-body: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-header
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-header req #"X-Custom" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format "  set-header: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-helpers ()
  (io:format "~nHelper functions performance:~n")

  (let* ((req (http.request:new "http://example.com"))
         (json #"{\"key\":\"value\"}")
         (iterations 10000))

    ;; set-json
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-json req json))
                 (lists:seq 1 iterations))))))
      (io:format "  set-json: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))

    ;; add-query-param
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:add-query-param req #"key" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format "  add-query-param: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))

;;; ---------------------------------------------------------------------------
;;; Helper functions for file output
;;; ---------------------------------------------------------------------------

(defun format-timestamp ()
  "Format current timestamp as ISO 8601 string"
  (let* ((now (calendar:universal_time))
         (`#(#(,year ,month ,day) #(,hour ,min ,sec)) now))
    (io_lib:format "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC"
                   (list year month day hour min sec))))

(defun bench-construction-to-device (device)
  "Run construction benchmarks and write to device"
  (io:format device "~nRequest construction performance:~n" '())

  ;; Simple URL only
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format device "  new/1 (URL only): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; With method
  (let* ((iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com"))
                (lists:seq 1 iterations))))))
    (io:format device "  new/2 (method + URL): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Full construction
  (let* ((iterations 10000)
         (headers #m(#"X-Custom" #"value"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST" "http://example.com" #"body" headers))
                (lists:seq 1 iterations))))))
    (io:format device "  new/4 (full): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun bench-setters-to-device (device)
  "Run setter benchmarks and write to device"
  (io:format device "~nSetter operations performance:~n" '())

  (let* ((req (http.request:new "http://example.com"))
         (iterations 100000))

    ;; set-method
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-method req #"POST"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-method: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-body
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-body req #"body"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-body: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

    ;; set-header
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-header req #"X-Custom" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-header: ~p iterations in ~pμs (~.2fns/op)~n"
                 (list iterations elapsed (/ (* elapsed 1000.0) iterations))))))

(defun bench-helpers-to-device (device)
  "Run helper function benchmarks and write to device"
  (io:format device "~nHelper functions performance:~n" '())

  (let* ((req (http.request:new "http://example.com"))
         (json #"{\"key\":\"value\"}")
         (iterations 10000))

    ;; set-json
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:set-json req json))
                 (lists:seq 1 iterations))))))
      (io:format device "  set-json: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))

    ;; add-query-param
    (let ((`#(,_ ,elapsed)
           (http.util:measure
             (lambda ()
               (lists:foreach
                 (lambda (_) (http.request:add-query-param req #"key" #"value"))
                 (lists:seq 1 iterations))))))
      (io:format device "  add-query-param: ~p iterations in ~pμs (~.2fμs/op)~n"
                 (list iterations elapsed (/ elapsed iterations))))))
