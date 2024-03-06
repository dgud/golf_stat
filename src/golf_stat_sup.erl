%%%-------------------------------------------------------------------
%% @doc golf_stat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(golf_stat_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Arg) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Arg).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Arg) ->
    Server = #{id => golf_stat,
               start => {golf_stat, start_link, [Arg]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [golf_stat]},
    {ok, {{one_for_all, 0, 1}, [Server]}}.

%%====================================================================
%% Internal functions
%%====================================================================
