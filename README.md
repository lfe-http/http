# http

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*General purpose data, functions, and utilities for use by LFE HTTP clients, servers, URL-parsers, web frameworks, etc.*

[![Project Logo][logo]][logo-large]

##### Table of Contents

* [Features](#features-)
* [Examples](#examples-)
* [Installation](#installation-)
* [License](#license-)

## Features [&#x219F;](#table-of-contents)

* **Binary-first design**: All strings are binaries by default for optimal performance
* **Fluent builder pattern**: Chain operations for readable code
* **Request maps**: `(http.request:new #"GET" "http://example.com")`
* **Response helpers**: `(http.response:ok #"Hello, World!")`, `(http.response:json 200 data)`
* **Smart headers**: Case-insensitive lookups, single-pass conversions
* **Erlang httpc interop**: `(http.c:request "http://google.com")`
* **150+ status codes**: `(http.status:im-a-teapot)`, `(http.status:code->message 200)`
* **2000+ MIME types**: `(http.mimetype:from-extension "json")`
* **Query parameters**: Automatic parsing and reconstruction
* **Content helpers**: `set-json`, `set-form`, `set-text` for common formats

## Examples [&#x219F;](#table-of-contents)

### Basic HTTP Client Requests

Start the required applications:

```lfe
(inets:start)
(ssl:start)
```

Simple GET request:

```lfe
> (http.c:request "https://httpbin.org/get")
#(ok #m(status 200
        headers #m(#"Content-Type" #"application/json" ...)
        body #"..."
        version 1.1))
```

GET request with custom headers:

```lfe
> (let* ((req (http.request:new "https://httpbin.org/headers"))
         (req-with-headers (http.request:set-header req #"User-Agent" #"lfe-http/1.0")))
    (http.c:request req-with-headers))
#(ok #m(status 200 ...))
```

POST request with JSON:

```lfe
> (let* ((body #"{\"name\":\"LFE\",\"type\":\"language\"}")
         (req (http.request:new #"POST" "https://httpbin.org/post"))
         (req-with-json (http.request:set-json req body)))
    (http.c:request req-with-json))
#(ok #m(status 200 ...))
```

POST with custom headers and body:

```lfe
> (let* ((headers (http.header:add (http.header:new)
                                    #"Content-Type"
                                    #"application/x-www-form-urlencoded"))
         (body #"key1=value1&key2=value2"))
    (http.c:request #"POST" "https://httpbin.org/post" body headers))
#(ok #m(status 200 ...))
```

### Working with Headers

Create a new headers map:

```lfe
> (http.header:new)
#m()
```

Add headers:

```lfe
> (let* ((h1 (http.header:new))
         (h2 (http.header:add h1 #"Content-Type" #"application/json"))
         (h3 (http.header:add h2 #"Accept" #"application/json")))
    h3)
#m(#"Content-Type" #"application/json"
   #"Accept" #"application/json")
```

Convert from a property list:

```lfe
> (http.header:from-list '(#(#"Content-Type" #"text/plain")
                            #(#"Accept" #"*/*")))
#m(#"Content-Type" #"text/plain"
   #"Accept" #"*/*")
```

Get header values (case-sensitive):

```lfe
> (let ((headers (http.header:from-list '(#(#"Content-Type" #"application/json")))))
    (http.header:get headers #"Content-Type"))
#"application/json"
```

Get header values (case-insensitive):

```lfe
> (let ((headers (http.header:from-list '(#(#"Content-Type" #"application/json")))))
    (http.header:get headers #"content-type" 'undefined #m(case-insensitive true)))
#"application/json"
```

Merge headers:

```lfe
> (let ((h1 (http.header:from-list '(#(#"Accept" #"application/json"))))
        (h2 (http.header:from-list '(#(#"Content-Type" #"text/plain")))))
    (http.header:merge h1 h2))
#m(#"Accept" #"application/json"
   #"Content-Type" #"text/plain")
```

## Installation [&#x219F;](#table-of-contents)

Add it to your `rebar.config` deps:

```erlang
{deps, [
  ...
  {http, "1.0.0", {pkg, lfe_http}}
]}.
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2023-2025, Duncan McGreggor <oubiwann@gmail.com>.

[//]: ---Named-Links---

[logo]: https://avatars1.githubusercontent.com/u/3434967?s=250
[logo-large]: https://avatars1.githubusercontent.com/u/3434967
[gh-actions-badge]: https://github.com/lfex/http/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/http/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.2-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-24%20to%2028-blue.svg
[versions]: https://github.com/lfex/http/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/lfex/http/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfex/http.svg
