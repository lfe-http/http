# http

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*General purpose data, functions, and utilities for use by LFE HTTP clients, servers, URL-parsers, web frameworks, etc.*

[![Project Logo][logo]][logo-large]

##### Table of Contents

* [Features](#features-)
* [Installation](#installation-)
* [License](#license-)

## Features [&#x219F;](#table-of-contents)

* Request maps: `(http.request:new 'get "http://example.com")`
* Response maps: `(http.response:new 200 #m(#"content-type" #"text-plain") #"")`
* Headers: `(http.header:list->map '(#(content-type #"text/plain")))`
* Interoperability with the Erlang stdlib `httpc` library: `(http.c:request "http://google.com")`
* +150 status codes: `(http.status:im-a-teapot)`
* +2000 mime types: `(http.mimetype:application/json))`

## Installation [&#x219F;](#table-of-contents)

Add it to your ``rebar.config`` deps:

```erlang
{deps, [
  ...
  {http, "0.5.1", {pkg, lfe_http}}
]}.
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2023-2024, Duncan McGreggor <oubiwann@gmail.com>.

[//]: ---Named-Links---

[logo]: https://avatars1.githubusercontent.com/u/3434967?s=250
[logo-large]: https://avatars1.githubusercontent.com/u/3434967
[gh-actions-badge]: https://github.com/lfex/http/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/http/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfex/http/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/lfex/http/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfex/http.svg
