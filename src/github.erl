-module(github).

-include_lib("riak_pipe/include/riak_pipe.hrl").

-export([preferences/1]).
-export([preferences/2]).
-export([start/0]).
-export([stop/0]).

-export([access_github_user/3,
	 access_github_stars/3,
	 extract_repo_info/3,
	 reduce_repo_langs/4]).

-define(HEADERS(), [{"Authorization",
		     "Basic " ++ atom_to_list(element(2, application:get_env(github, auth)))}]).
-define(COLLECT_TIMEOUT, 100000).

%% ===================================================================
%% Public API
%% ===================================================================

access_github_user(User, Partition, Fitting) ->
    Url = "https://api.github.com/users/" ++ User ++ "/starred",
    {ok, {{_, 200, _}, Headers, _Body}} = httpc:request(head, {Url, ?HEADERS()}, [], []),
    Link = proplists:get_value("link", Headers),
    case re:run(Link, "page=(\\d?)", [global, {capture, all, list}]) of
	{match, [_, [_, Pages]]} ->
	    lists:foreach(fun(Page) ->
				  ok = riak_pipe_vnode_worker:send_output({User, Page},
									  Partition,
									  Fitting)
			  end, lists:seq(1, list_to_integer(Pages)));
	nomatch ->
	    ok
    end.

access_github_stars({User, Page}, Partition, Fitting) when is_integer(Page) ->
    access_github_stars({User, integer_to_list(Page)}, Partition, Fitting);
access_github_stars({User, Page}, Partition, Fitting) ->
    Url = "https://api.github.com/users/" ++ User ++ "/starred?page=" ++ Page,
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, ?HEADERS()}, [], []),
    lists:foreach(fun({Repo}) ->
			  LanguagesUrl = proplists:get_value(<<"languages_url">>, Repo),
			  ok = riak_pipe_vnode_worker:send_output(LanguagesUrl,
								  Partition,
								  Fitting)
		  end, jiffy:decode(Body)).
 
extract_repo_info(Url, Partition, Fitting) when is_binary(Url) ->
    extract_repo_info(binary_to_list(Url), Partition, Fitting);
extract_repo_info(LanguagesUrl, Partition, Fitting) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {LanguagesUrl, ?HEADERS()}, [], []),
    {Langs} = jiffy:decode(Body),
    lists:foreach(fun({Lang, Count}) ->
			  ok = riak_pipe_vnode_worker:send_output({Lang, Count},
								  Partition,
								  Fitting)
		  end, Langs).

reduce_repo_langs(Lang, Counts, Partition, Fitting) ->
    {ok, [lists:sum(Counts)]}.

preferences(User) ->
    preferences(User, []).

preferences(User, Config) ->
    Options = case Config of
		  [] -> [];
		  [trace] -> [{trace, all}, {log, lager}]
	      end,
    Spec = [#fitting_spec{name="fetch number of stars pages",
			  module=riak_pipe_w_xform,
			  arg=fun ?MODULE:access_github_user/3,
			  chashfun=fun(User) ->
			  		   riak_core_util:chash_key({<<"pages">>, User})
			  	   end
			 },
	    #fitting_spec{name="fetch user stars list",
			  module=riak_pipe_w_xform,
			  arg=fun ?MODULE:access_github_stars/3,
			  chashfun=fun({_, Page}) ->
			  		   riak_core_util:chash_key({Page, Page})
			  	   end
			 },
	    #fitting_spec{name="fetch repo languages",
			  module=riak_pipe_w_xform,
			  arg=fun ?MODULE:extract_repo_info/3,
			  chashfun=fun(Repo) ->
					   riak_core_util:chash_key({<<"repo">>,
								     term_to_binary(Repo)})
				   end
			  },
	    #fitting_spec{name=langs_counter,
			  module=riak_pipe_w_reduce,
			  arg=fun ?MODULE:reduce_repo_langs/4,
			  chashfun=fun riak_pipe_w_reduce:chashfun/1}],
    {ok, Pipe} = riak_pipe:exec(Spec, Options),
    ok = riak_pipe:queue_work(Pipe, User),
    riak_pipe:eoi(Pipe),
    {eoi, Results, _} = riak_pipe:collect_results(Pipe, ?COLLECT_TIMEOUT),
    extract_languages_stat([{Name, Counter} || {langs_counter, {Name, [Counter]}} <- Results]).

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
    application:start(github).

stop() ->
    application:stop(github).
