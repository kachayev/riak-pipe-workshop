-module(github).

-include_lib("riak_pipe/include/riak_pipe.hrl").

-export([preferences/1]).
-export([preferences/2]).
-export([start/0]).
-export([stop/0]).

-export([access_github_user/2,
     access_github_stars/2,
     extract_repo_langs/2,
     reduce_repo_langs/2]).

-define(COLLECT_TIMEOUT, 100000).
-define(HEADERS(), [{"Authorization",
            "Basic " ++ atom_to_list(element(2, application:get_env(github, auth)))},
            {"User-Agent", "riak_pipe_workshop"}]).

%% ===================================================================
%% Public API
%% ===================================================================

preferences(User) ->
    preferences(User, []).

preferences(User, Config) ->
    %% define new riak_pipe
    Spec = [
        stream_to(num_pages, fun access_github_user/2, group_to(<<"user">>)),
        stream_to(fetch_page, fun access_github_stars/2, group_to(<<"page">>)),
        stream_to(langs_repo, fun extract_repo_langs/2, group_to(<<"repo">>)),
        reduce(langs_counter, fun reduce_repo_langs/2)
       ],
    Options = case Config of
        [] -> [];
        [trace_github] -> [{trace, [github]}, {log, lager}];
        [trace] -> [{trace, all}, {log, lager}]
    end,
    {ok, Pipe} = riak_pipe:exec(Spec, Options),

    % wait for all vnodes to be up
    riak_core:wait_for_service(riak_pipe),

    %% send username to the first stage
    ok = riak_pipe:queue_work(Pipe, User),

    %% send end-of-input signal
    riak_pipe:eoi(Pipe),

    %% collect results
    {eoi, Results, _} = riak_pipe:collect_results(Pipe, ?COLLECT_TIMEOUT),

    %% calculate percents for each language and sort list (from top to bottom)
    extract_languages_stat([{Name, Counter} || {langs_counter, {Name, [Counter]}} <- Results]).

%% ===================================================================
%% Stages/fitting functions
%% ===================================================================

%% stage 1
access_github_user(Fitting, User) ->
    Url = "https://api.github.com/users/" ++ User ++ "/starred",
    riak_pipe_log:trace(Fitting, [github], [fetching, Url]),
    {ok, {{_, 200, _}, Headers, _Body}} = httpc:request(head, {Url, ?HEADERS()}, [], []),
    Link = proplists:get_value("link", Headers),
    case re:run(Link, "page=(\\d?)", [global, {capture, all, list}]) of
	{match, [_, [_, Pages]]} ->
	    lists:map(fun(Page) ->
			      {User, integer_to_list(Page)}
		      end, lists:seq(1, list_to_integer(Pages)));
	nomatch -> []
    end.

%% stage 2
access_github_stars(Fitting, {User, Page}) ->
    Url = "https://api.github.com/users/" ++ User ++ "/starred?page=" ++ Page,
    riak_pipe_log:trace(Fitting, [github], [fetching, Url]),
    Repos = fetch_json(Url),
    lists:map(fun({Repo}) ->
		      LanguagesUrl = proplists:get_value(<<"languages_url">>, Repo),
		      binary_to_list(LanguagesUrl)
	      end, Repos).

%% stage 3
extract_repo_langs(Fitting, LanguagesUrl) ->
    riak_pipe_log:trace(Fitting, [github], [fetching, LanguagesUrl]),
    %% json format is {[{Lang :: binary(), Count :: integer()}]}
    element(1, fetch_json(LanguagesUrl)).

%% stage 4
reduce_repo_langs(_Lang, Counts) ->
    {ok, [lists:sum(Counts)]}.

%% ===================================================================
%% Helpers
%% ===================================================================

streamer(Fun) ->
    fun(Args, Partition, Fitting) ->
	    lists:foreach(fun(El) ->
				  %% xxx: this may fail, will resolve in future
				  ok = riak_pipe_vnode_worker:send_output(El, Partition, Fitting)
			  end, Fun(Fitting, Args))
    end.

reducer(Fun) ->
    fun(Key, Value, _Partition, _Fitting) -> Fun(Key, Value) end.

stream_to(Name, Fun, ChashFun) ->
    #fitting_spec{name=Name, module=riak_pipe_w_xform, arg=streamer(Fun), chashfun=ChashFun}.

reduce(Name, Fun) ->
    ChashFun = fun riak_pipe_w_reduce:chashfun/1,
    #fitting_spec{name=Name, module=riak_pipe_w_reduce, arg=reducer(Fun), chashfun=ChashFun}.

%% as you see, I really miss Haskell-style currying :)
group_to(Key) ->
    fun(Output) -> riak_core_util:chash_key({Key, term_to_binary(Output)}) end.

fetch_json(Url) ->
    %% xxx: very naive approach
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, ?HEADERS()}, [], []),
    jiffy:decode(Body).

extract_languages_stat(All) ->
    extract_languages_stat(All, lists:sum([C || {_, C} <- All])).

extract_languages_stat(All, Total) ->
    lists:sort(fun({_, Left}, {_, Right}) -> Left > Right end,
	       [{Name, Counter/Total  * 100} || {Name, Counter} <- All]).

%% ===================================================================
%% Application API
%% ===================================================================

start() ->
    ok = application:start(lager),
    ok = application:start(asn1),
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(os_mon),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(mochiweb),
    ok = application:start(webmachine),
    ok = application:start(riak_sysmon),
    ok = application:start(riak_core),
    ok = application:start(riak_pipe),
    ok = application:start(github),
    {ok, _} = application:get_env(github, auth).

stop() ->
    application:stop(github).
