%%%-------------------------------------------------------------------
%% @doc nova_admin public API
%% @end
%%%-------------------------------------------------------------------

-module(golf_stat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("golf_stat.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
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
    golf_stat_sup:start_link(Arg).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
