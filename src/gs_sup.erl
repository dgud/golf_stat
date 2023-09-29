%%%-------------------------------------------------------------------
%%% @author Dan <dgud@Dan>
%%% @copyright (C) 2023, Dan
%%% @doc
%%%       Starts the application including the web-server
%%% @end
%%% Created : 30 Aug 2023 by Dan <dgud@Dan>
%%%-------------------------------------------------------------------
-module(gs_sup).

-behaviour(supervisor).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-include("golf_stat.hrl").
-define(DEFAULT_PORT, 21137).


start(_StartType, StartArgs) ->
    %% ?DBG("START: ~p ~p~n",[_StartType, StartArgs]),
    try
        Arg = case proplists:get_value(dir, StartArgs) of
                  undefined ->
                      case os:getenv("GS_DIR") of
                          false ->
                              ?LOG_CRITICAL("No DIR found set GS_DIR environment variable~n", []),
                              throw({error, no_dir});
                          Dir ->
                              #{dir => Dir}
                      end;
                  Dir ->
                      #{dir => Dir}
              end,
        {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, Arg),
        ok = gs_rest_handler:init_cowboy(?DEFAULT_PORT),
        ?LOG_NOTICE("Starting: http://localhost:~w~n~n", [?DEFAULT_PORT]),
        {ok, Pid}
    catch Error ->
            Error
    end.

stop(_Ref) ->
    ok = cowboy:stop_listener(my_http_listener),
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Arg) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 5},
    Server = #{id => golf_stat,
               start => {golf_stat, start_link, [Arg]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [golf_stat]},
    {ok, {SupFlags, [Server]}}.
