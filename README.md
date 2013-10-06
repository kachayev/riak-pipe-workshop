Riak Pipes Workshop (code)
==========================

What do you need
----------------

`Erlang`, `rebar`, Github user (to make authenticated requests to Github API), a bit of free time and willing to build distributed data processing system.

The task
--------

Imagine that you want to calculate what programming langauges are most interesting for concrete Github user ("language preferences"). There is no obvious way of how to do this, but we will choose next one: calculate total proportions of all language that were used to implement all project starred by concrete user. To get more information about task as well as clean vision of how to use Github API to perform all necessary requests, see workshop slides (todo: add link to slides).

This computation fits perfectly to `riak_pipe` architecture: message-based streaming from one part of your application to another. We will use next "stages" to build pipe:

**Stage 1**. Get number of pages in listing of starred projects

**Stage 2**. Fetch each page (more info about API endpoint ["List repositories being starred"](http://developer.github.com/v3/activity/starring/)) and extract "languages_url" for each repo

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
> Authorization: Basic __YOUR_GITHUB_AUTH__
```

If you are ready we can move on:

```shell
$ GITHUB_AUTH=__YOUR_GITHUB_AUTH__ make deps compile console
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

You will see much more information, I removed all lager messages to show the key idea. If you want to get all information about what's going on, use `trace` option:

```shell
$ GITHUB_AUTH=__YOUR_GITHUB_AUTH__ make console
(github@127.0.0.1)1> github:preferences("kachayev", [trace]).
20:09:16.183 [info] langs_counter: {trace,all,{fitting,init_started}}
20:09:16.183 [info] langs_counter: {trace,all,{fitting,init_finished}}
20:09:16.183 [info] fetch repo languages: {trace,all,{fitting,init_started}}
20:09:16.183 [info] fetch repo languages: {trace,all,{fitting,init_finished}}
20:09:16.184 [info] fetch user stars list: {trace,all,{fitting,init_started}}
20:09:16.184 [info] fetch user stars list: {trace,all,{fitting,init_finished}}
20:09:16.184 [info] fetch number of stars pages: {trace,all,{fitting,init_started}}
20:09:16.184 [info] fetch number of stars pages: {trace,all,{fitting,init_finished}}
20:09:16.190 [info] fetch number of stars pages: {trace,all,{fitting,{get_details,#Ref<0.0.0.759>,456719261665907161938651510223838443642478919680,...}}}
20:09:16.192 [info] fetch number of stars pages: {trace,all,{vnode,{start,456719261665907161938651510223838443642478919680}}}
20:09:16.193 [info] fetch number of stars pages: {trace,all,{vnode,{queued,456719261665907161938651510223838443642478919680,[...]}}}
...
```

Tags
----

If you are new in `riak_pipe` I suggest you to start browsing code from `v1.0` tag instead of `master` branch. There are many lines of boilerplate code in initial implementation but it's really much more easy to undestand what's going on. Latest tags are mostly about high-level patterns instead of low-level details.

* [v1.0](https://github.com/kachayev/riak-pipe-workshop/tree/v1.0) - Rough implementation that you can run and test

* [v1.1](https://github.com/kachayev/riak-pipe-workshop/tree/v1.1) - Code cleanup

* v1.2 - [todo] Local caching in order to run tests without connection to internet

* v2.0 - [todo] Work with backpresure facilities, tracing / logging configuration

* v2.1 - [todo] Error propagation

* v2.2 - [todo] Collect stat about pipes/fittings/queues

* v2.3 - [todo] Built-in fitting modules

* v2.4 - [todo] Implement your own fitting module, when/why/how

* v3.0 - [todo] Multi-nodes projects, cluster management, queues handoff

* v3.1 - [todo] Testing in multi-nodes environment

* v4.0 - [todo] Simples web-based interface for `github` module calls

Licence
-------

**MIT**

For more information, see LICENCE file.