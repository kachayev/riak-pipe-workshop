Riak Pipes Workshop (code)
==========================

What do you need
----------------

`Erlang`, `rebar`, Github user (to make authenticated requests to Github API), a bit of free time and willing to build distributed data processing system.

The task
--------

Imagine that you want to calculate what programming langauges are most interesting for concrete Github user. There is no obvious way of how to do this, but we will choose next one: calculate total proportions of all language that were used to implement all project starred by concrete user.

This computation fits perfectly to `riak_pipe` architecture: message-based streaming from one part of your application to another. We will use next "stages" to build pipe:

**Stage 1**. Get number of pages in listing of starred projects

**Stage 2**. Fetch each page (more info about API endpoint [http://developer.github.com/v3/activity/starring/]("List repositories being starred")) and extract "languages_url" for each repo

**Stage 3**. Fetch each "languages_url" and send each language to reducer

**Stage 4**. Receive info about all languages and calculate percent of mentions for each.

Note, that **Stage 2** and **Stage 3** are naturally distributable among independent workers.

Run and test
------------

For the first, you need to specify Basic Auth header to work with Github. Run this is terminal:

```shell
$ curl -i -vvv -u YOUR_GITHUB_USER:YOUR_GITHUB_PASSWORD https://api.github.com
```

Find next lines:

```shell
> GET / HTTP/1.1
> Authorization: Basic GITHUB_AUTH
```

If you are ready we can move on:

```shell
$ GITHUB_AUTH=<...> make deps compile console
erl \
		-name github@127.0.0.1 \
		-setcookie github-users-analyzer \
		-riak_core handoff_port 4090 \
		-riak_core web_port 4091 \
		-sasl errlog_type error \
		-pa deps/*/ebin ebin \
		-github auth GITHUB_AUTH
		-s github
Eshell V5.9.3.1  (abort with ^G)
(github@127.0.0.1)1> github:preferences("kachayev").
[{<<"Erlang">>,28.906233446829255},
 {<<"Scala">>,12.696560617387881},
 {<<"Python">>,11.733369970253419},
 {<<"C">>,10.98499886188494},
 {<<"Haskell">>,5.9686012347566795},
 {<<"JavaScript">>,5.760862852309727},
 {<<"Rust">>,4.063551478964284},
...
(github@127.0.0.1)2>
```

P.S. You will see much more information, I removed all lager messages to show the key idea.

Tags
----

* v1.0 - Rough implementation that you can run and test

* v1.1 - Code cleanup

* v2.0 - Work with backpresure facilities, tracing / logging configuration

* v2.1 - Error propagation

* v3.0 - Multi-nodes projects, cluster management, queues handoff

* v3.1 - Testing in multi-nodes environment

Licence
-------

MIT

For more information, see LICENCE file.