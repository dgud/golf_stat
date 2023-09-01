%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson
%%% @copyright (C) 2023, Dan Gudmundsson
%%% @doc
%%%        Server holding data and answering calls
%%% @end
%%% Created : 30 Aug 2023 by Dan 
%%%-------------------------------------------------------------------
-module(golf_stat).

-behaviour(gen_server).

%% API

-export([courses/0, course/1, add_course/1]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("golf_stat.hrl").
-define(SERVER, ?MODULE).

-record(state, {dir = "/tmp", courses = []}).

-import(gs_lib, [json_encode/1, json_decode/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec courses() -> [CourseName::string()].
courses() ->
    gen_server:call(?SERVER, courses).

-spec course(Id :: integer()) -> {ok, Course::map()} | {error, binary()}.
course(Id) ->
    gen_server:call(?SERVER, {course, Id}).

-spec add_course(Id :: integer() | string()) ->
          {ok, [CourseName::string()]} | {error, binary()}.
add_course(#{name := Bin, pars := List}=Course)
  when is_binary(Bin), byte_size(Bin) > 3, is_list(List), length(List) >= 9 ->
    case lists:all(fun is_integer/1, List) of
        true ->
            gen_server:call(?SERVER, {add_course, Course});
        false ->
            {error, <<"Bad course pars"/utf8>>}
    end;
add_course(_Bad) ->
    ?DBG("~p", [_Bad]),
    {error, <<"Bad course data"/utf8>>}.

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(#{dir := Dir} = _Args) ->
    process_flag(trap_exit, true),
    %% ?DBG("~p~n",[_Args]),
    Cs = gs_stats:read_courses(Dir),
    {ok, #state{dir = Dir, courses = Cs}}.

handle_call(courses, _From, #state{courses = Cs} = State) ->
    Reply = [Name || #{name := Name} <- Cs],
    {reply, Reply, State};

handle_call({course, Id}, _From, #state{courses = Cs} = State) ->
    if is_integer(Id) ->
            try
                Course = #{} = lists:nth(Id+1, Cs),
                {reply, {ok, Course}, State}
            catch _:_ ->
                    IdStr = integer_to_binary(Id),
                    {reply, {error, <<"Could not find course id: ", IdStr/binary>>}, State}
            end;
       is_binary(Id) ->
            case lists:search(fun(#{name:=Name}) -> string:equal(Name, Id, true) end, Cs) of
                {value, Course} ->
                    {reply, {ok, Course}, State};
                false ->
                    {reply, {error, <<"Could not find course id: ", Id/binary>>}, State}
            end;
       true ->
            ?DBG("Bad Id: ~p~n", [Id]),
            {reply, {error, <<"Bad course string">>}, State}
    end;

handle_call({add_course, Course}, _From, #state{courses = Cs, dir = Dir} = State) ->
    Id = maps:get(name, Course),
    case lists:any(fun(#{name:=Name}) -> string:equal(Name, Id, true) end, Cs) of
        true ->
            {reply, {error, <<"Already exists"/utf8>>}, State};
        false ->
            NewCs = gs_stats:save_courses([Course|Cs], Dir),
            Names = [Name || #{name := Name} <- NewCs],
            {reply, {ok, Names}, State#state{courses = NewCs}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
