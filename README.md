# http

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

*General purpose data, functions, and utilties for use by LFE HTTP clients, servers, URL-parsers, web frameworks, etc.*

[![Project Logo][logo]][logo-large]

##### Table of Contents

* [Features](#features-)
* [Installation](#installation--)
* [License](#license-)

## Features [&#x219F;](#table-of-contents)

* Status codes: `(http.code:im-a-teapot)`
* Request map: `(http.request:new 'GET "http://example.com")`
* Headers: `(http.header:list->map (#(content-type #"text/plain")))`

## Installation [&#x219F;](#contents)

Add it to your ``rebar.config`` deps:

```erlang
{deps, [
  ...
  {http, "0.2.0", {pkg, lfe_http}}
]}.
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2023, Duncan McGreggor <oubiwann@gmail.com>.

[//]: ---Named-Links---

[logo]: https://avatars1.githubusercontent.com/u/3434967?s=250
[logo-large]: https://avatars1.githubusercontent.com/u/3434967
[gh-actions-badge]: https://github.com/lfex/http/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/http/actions
[org]: https://github.com/lfex
[github]: https://github.com/lfex/http
[gitlab]: https://gitlab.com/lfex/http
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfex/http/blob/master/.github/workflows/cicd.yml
[github tags]: https://github.com/lfex/http/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/http.svg
[github downloads]: https://img.shields.io/github/downloads/lfex/http/total.svg
[hex badge]: https://img.shields.io/hexpm/v/http.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/http
[hex downloads]: https://img.shields.io/hexpm/dt/http.svg
