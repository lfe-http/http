(defmodule full-cycle-bench
  (export (run 0)))

(defun run ()
  (io:format "~n=== Full Request/Response Cycle Benchmarks ===~n")

  ;; Simple GET request construction + conversion
  (io:format "~nGET request (construction + Erlang conversion):~n")
  (let* ((iterations 1000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let ((req (http.request:new #"GET" "http://example.com")))
                    (http.c:->erlang req)))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; POST with body and headers
  (io:format "~nPOST request (full construction + conversion):~n")
  (let* ((iterations 1000)
         (body #"{\"key\":\"value\"}")
         (headers #m(#"Content-Type" #"application/json"
                     #"Authorization" #"Bearer token"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let ((req (http.request:new #"POST"
                                                "http://example.com/api"
                                                body
                                                headers)))
                    (http.c:->erlang req)))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Response parsing
  (io:format "~nResponse parsing (Erlang -> LFE):~n")
  (let* ((iterations 1000)
         (httpc-resp `#(#(#(1 1) 200 #"OK")
                        ((#"content-type" "application/json")
                         (#"content-length" "50"))
                        #"{\"response\":\"data\"}"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.c:erlang-> httpc-resp))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Complete cycle simulation
  (io:format "~nComplete cycle (request build + convert + parse response):~n")
  (let* ((iterations 1000)
         (httpc-resp `#(#(#(1 1) 200 #"OK")
                        ((#"content-type" "application/json"))
                        #"{\"result\":\"success\"}"))
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let* ((req (http.request:new #"GET" "http://example.com"))
                         (_ (http.c:->erlang req))
                         (resp (http.c:erlang-> httpc-resp)))
                    resp))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
