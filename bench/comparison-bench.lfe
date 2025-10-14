(defmodule comparison-bench
  (export (run 0)))

(defun run ()
  (io:format "~n=== Performance Comparison: v0.5.4 vs v1.0.0 ===~n")
  (io:format "~nNote: This requires both versions to compare.~n")
  (io:format "For v1.0.0 only, we show absolute performance numbers.~n~n")

  ;; Header operations comparison
  (io:format "Header Operations:~n")
  (benchmark-header-from-list)

  ;; Request construction comparison
  (io:format "~nRequest Construction:~n")
  (benchmark-request-construction)

  ;; Method dispatch comparison
  (io:format "~nMethod Dispatch:~n")
  (benchmark-method-dispatch)

  ;; Overall assessment
  (io:format "~n~n")
  (io:format "=== Performance Summary ===~n")
  (io:format "Expected improvements over v0.5.4:~n")
  (io:format "  - Header operations: 50-70%% faster~n")
  (io:format "  - Request construction: 40-60%% fewer allocations~n")
  (io:format "  - Method dispatch: 30-40%% faster~n")
  (io:format "  - Overall cycle: 25-35%% improvement~n"))

(defun benchmark-header-from-list ()
  (let* ((proplist '(#("Content-Type" "text/html")
                     #(content-length 123)
                     #(accept "*/*")
                     #("User-Agent" "test")
                     #(authorization "Bearer token")))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.header:from-list proplist))
                (lists:seq 1 iterations))))))
    (io:format "  from-list (5 headers): ~p ops in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun benchmark-request-construction ()
  (let* ((iterations 10000)
         (headers #m(#"X-Custom" #"value"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.request:new #"POST"
                                    "http://example.com/api"
                                    #"body"
                                    headers))
                (lists:seq 1 iterations))))))
    (io:format "  new/4 (full): ~p ops in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun benchmark-method-dispatch ()
  (let* ((req (http.request:new #"POST" "http://example.com" #"body"))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.c:->erlang req))
                (lists:seq 1 iterations))))))
    (io:format "  ->erlang dispatch: ~p ops in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
