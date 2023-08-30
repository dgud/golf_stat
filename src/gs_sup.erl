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
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", gs_rest_handler, [hello]},
                                             {"/courses", gs_rest_handler, [courses]},
                                             {"/course/:courseId", gs_rest_handler, [course]}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, ?DEFAULT_PORT}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares => [cowboy_router, cowboy_handler]
                                  }),
    %% ?DBG("START: ~p ~p~n",[_StartType, StartArgs]),
    Arg = case proplists:get_value(dir, StartArgs) of
              undefined -> #{dir => os:getenv("GS_DIR")};
              Dir -> #{dir => Dir}
          end,
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, Arg),
    {ok, Pid}.

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
