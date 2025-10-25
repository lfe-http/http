(defmodule header-bench
  (export
   (run 0)
   (bench-from-list 0)
   (bench-add-headers 0)
   (bench-get-ci 0)
   (bench-merge 0)))

(defun run ()
  (io:format "~n=== HTTP Header Benchmarks ===~n")
  (bench-from-list)
  (bench-add-headers)
  (bench-get-ci)
  (bench-merge))

(defun bench-from-list ()
  (io:format "~nfrom-list/1 performance (single-pass conversion):~n")

  ;; Small list (10 headers)
  (let* ((small-list '(#(content-type #"text/html")
                       #(accept #"application/json")
                       #(user-agent #"test/1.0")
                       #(host #"example.com")
                       #(authorization #"Bearer token")
                       #(x-custom-1 #"value1")
                       #(x-custom-2 #"value2")
                       #(x-custom-3 #"value3")
                       #(content-length #"1024")
                       #(cache-control #"no-cache")))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.header:from-list small-list))
                (lists:seq 1 iterations))))))
    (io:format "  Small list (10 headers): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Medium list (50 headers)
  (let* ((medium-list (lists:map
                       (lambda (n)
                         `#(,(list_to_binary (io_lib:format "header-~p" (list n)))
                            ,(list_to_binary (io_lib:format "value-~p" (list n)))))
                       (lists:seq 1 50)))
         (iterations 1000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.header:from-list medium-list))
                (lists:seq 1 iterations))))))
    (io:format "  Medium list (50 headers): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations))))

  ;; Large list (200 headers)
  (let* ((large-list (lists:map
                      (lambda (n)
                        `#(,(list_to_binary (io_lib:format "header-~p" (list n)))
                           ,(list_to_binary (io_lib:format "value-~p" (list n)))))
                      (lists:seq 1 200)))
         (iterations 100)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.header:from-list large-list))
                (lists:seq 1 iterations))))))
    (io:format "  Large list (200 headers): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))

(defun bench-add-headers ()
  (io:format "~nadd/3 performance:~n")

  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let ((hdrs (http.header:new)))
                    (http.header:add hdrs #"content-type" #"text/html")))
                (lists:seq 1 iterations))))))
    (io:format "  Binary keys: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let ((hdrs (http.header:new)))
                    (http.header:add hdrs "content-type" "text/html")))
                (lists:seq 1 iterations))))))
    (io:format "  String keys: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations))))

  (let* ((iterations 100000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (let ((hdrs (http.header:new)))
                    (http.header:add hdrs 'content-type 'text/html)))
                (lists:seq 1 iterations))))))
    (io:format "  Atom keys: ~p iterations in ~pμs (~.2fns/op)~n"
               (list iterations elapsed (/ (* elapsed 1000.0) iterations)))))

(defun bench-get-ci ()
  (io:format "~nget-ci/2 performance (case-insensitive lookup):~n")

  (let* ((hdrs (http.header:from-list
                '(#(Content-Type #"text/html")
                  #(Accept #"application/json")
                  #(User-Agent #"test/1.0")
                  #(Host #"example.com")
                  #(Authorization #"Bearer token"))))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  ;; Search for different cases
                  (http.header:get-ci hdrs #"content-type")
                  (http.header:get-ci hdrs #"ACCEPT")
                  (http.header:get-ci hdrs #"uSeR-aGeNt"))
                (lists:seq 1 iterations))))))
    (io:format "  ~p iterations (3 lookups each) in ~pμs (~.2fμs/lookup)~n"
               (list iterations elapsed (/ elapsed (* iterations 3)))))

  ;; Compare with case-sensitive lookup
  (let* ((hdrs (http.header:from-list
                '(#(content-type #"text/html")
                  #(accept #"application/json")
                  #(user-agent #"test/1.0"))))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_)
                  (http.header:get hdrs #"content-type")
                  (http.header:get hdrs #"accept")
                  (http.header:get hdrs #"user-agent"))
                (lists:seq 1 iterations))))))
    (io:format "  Case-sensitive get/2 (baseline): ~p iterations in ~pμs (~.2fμs/lookup)~n"
               (list iterations elapsed (/ elapsed (* iterations 3))))))

(defun bench-merge ()
  (io:format "~nmerge/2 performance:~n")

  (let* ((hdrs1 (http.header:from-list
                 '(#(content-type #"text/html")
                   #(accept #"*/*")
                   #(user-agent #"test/1.0")
                   #(host #"example.com")
                   #(x-custom-1 #"value1"))))
         (hdrs2 (http.header:from-list
                 '(#(content-type #"application/json")
                   #(authorization #"Bearer token")
                   #(x-custom-2 #"value2")
                   #(cache-control #"no-cache"))))
         (iterations 10000)
         (`#(,_ ,elapsed)
          (http.util:measure
            (lambda ()
              (lists:foreach
                (lambda (_) (http.header:merge hdrs1 hdrs2))
                (lists:seq 1 iterations))))))
    (io:format "  Merge two header maps (5+4 headers): ~p iterations in ~pμs (~.2fμs/op)~n"
               (list iterations elapsed (/ elapsed iterations)))))
