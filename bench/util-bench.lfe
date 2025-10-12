(defmodule util-bench
  (export
   (run 0)
   (bench-ensure-binary 0)
   (bench-binary-upcase 0)
   (bench-query-string 0)))

(defun run ()
  (io:format "~n=== HTTP Util Benchmarks ===~n")
  (bench-ensure-binary)
  (bench-binary-upcase)
  (bench-query-string))

(defun bench-ensure-binary ()
  (io:format "~nensure-binary/1 performance:~n")

  ;; Binary input (should be instant - zero-copy)
  (let* ((iterations 1000000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:ensure-binary #"test"))
                (lists:seq 1 iterations))))))
    (io:format "  Binary input: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

  ;; Atom input (conversion required)
  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:ensure-binary 'test))
                (lists:seq 1 iterations))))))
    (io:format "  Atom input: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

  ;; List input (conversion required)
  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:ensure-binary "test"))
                (lists:seq 1 iterations))))))
    (io:format "  List input: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

(defun bench-binary-upcase ()
  (io:format "~nbinary-upcase/1 performance:~n")

  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:binary-upcase #"content-type"))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

(defun bench-query-string ()
  (io:format "~nquery-string/1 performance:~n")

  ;; Small map (3 params)
  (let* ((params #m(#"key1" #"value1"
                    #"key2" #"value2"
                    #"key3" #"value3"))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:query-string params))
                (lists:seq 1 iterations))))))
    (io:format "  Small map (3 params): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

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
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.util:query-string params))
                (lists:seq 1 iterations))))))
    (io:format "  Larger map (10 params): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
