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

-export([courses/0, course/1, add_course/1,
         add_round/2,
         user_round_selection/1,
         user_diagram_data/2,
         user_stats_string/2]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-include("golf_stat.hrl").
-define(SERVER, ?MODULE).

-record(state, {dir = "/tmp", courses = [], users = []}).

-import(gs_lib, [json_encode/1, json_decode/2]).

-type userId() :: atom() | string() | binary().

-type bstring() :: unicode:unicode_binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec courses() -> [CourseName::string()].
courses() ->
    gen_server:call(?SERVER, courses).

-spec course(Id :: integer()) -> {ok, Course::map()} | {error, bstring()}.
course(Id) ->
    gen_server:call(?SERVER, {course, Id}).

-spec add_course(Id :: integer() | string()) ->
          {ok, [CourseName::bstring()]} | {error, bstring()}.
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

-spec user_round_selection(User :: userId()) -> {ok, [bstring()]}.
user_round_selection(User) ->
    gen_server:call(?SERVER, {user_round_selection, to_user_atom(User)}).

-spec user_diagram_data(User :: userId(), Sel::bstring()) -> {ok, {term(), [bstring()]}}.
user_diagram_data(User, Selection) ->
    gen_server:call(?SERVER, {user_diagram_data, to_user_atom(User), Selection}).

-spec user_stats_string(User :: userId(), Sel::bstring()) -> {ok, bstring()}.
user_stats_string(User, Selection) ->
    gen_server:call(?SERVER, {user_stats_string, to_user_atom(User), Selection}).

-spec add_round(User :: userId(), Round::map()) -> {ok, bstring()}.
add_round(User, Round) when is_map(Round) ->
    gen_server:call(?SERVER, {add_round, to_user_atom(User), Round}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(#{dir := Dir} = _Args) ->
    process_flag(trap_exit, true),
    %% ?DBG("~p~n",[_Args]),
    Cs = gs_stats:read_courses(Dir),
    Files = filelib:wildcard(filename:join(Dir, "*_stat.json")),
    Load = fun(File, Acc) ->
                   [User|_] = string:split(filename:basename(File), "_stat.json"),
                   UserData = #{
                                file   => File,
                                rounds => gs_stats:read_player(File)
                               },
                   Acc#{to_user_atom(User) => UserData}
           end,
    UserData = lists:foldl(Load, #{}, Files),
    io:format("Loading users: ~p~n", [maps:keys(UserData)]),
    {ok, #state{dir = Dir, courses = Cs, users = UserData}}.

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

handle_call({user_round_selection, User}, _From, #state{users = Users} = State) ->
    Res = case maps:get(User, Users, undefined) of
              undefined ->
                  {error, <<"No such user: ", (atom_to_binary(User))/binary >>};
              #{rounds := Rounds} ->
                  {ok, default_menus(Rounds) ++ [round_id(Course) || Course <- Rounds]}
          end,
    {reply, Res, State};

handle_call({user_stats_string, User, SelString}, _From, #state{users = Users} = State) ->
    Res = case maps:get(User, Users, undefined) of
              undefined ->
                  {error, <<"No such user: ", (atom_to_binary(User))/binary >>};
              #{rounds := Rounds} ->
                  case split_round_data(SelString, Rounds) of
                      {Desc, SelRounds, Rest} ->
                          Doc = gs_stats:print_stats(Desc, SelRounds, Rest),
                          {ok, unicode:characters_to_binary(Doc)};
                      not_found ->
                          {error, <<"No such round">>}
                  end
          end,
    {reply, Res, State};

handle_call({user_diagram_data, User, SelString}, _From, #state{users = Users} = State) ->
    Res = case maps:get(User, Users, undefined) of
              undefined ->
                  {error, <<"No such user: ", (atom_to_binary(User))/binary >>};
              #{rounds := Rounds} ->
                  case split_round_data(SelString, Rounds) of
                      {Desc, SelRounds, Rest} ->
                          {Data, Labels} = diagram_data(Desc, SelRounds, Rest),
                          {ok, {Labels, Data}};
                      not_found ->
                          {error, <<"No such round">>}
                  end
          end,
    {reply, Res, State};

handle_call({add_round, User, Round}, _From, #state{users = Users} = State) ->
    case maps:get(User, Users, undefined) of
        undefined ->
            Res = {error, <<"No such user: ", (atom_to_binary(User))/binary >>},
            {reply, Res, State};
        #{file := File, rounds := Rounds} = UserData ->
            NewRounds = [Round|Rounds],
            ok = gs_stats:save_player(File, sort_rounds(NewRounds)),
            Res = {ok, round_id(Round)},
            {reply, Res, State#state{users = Users#{User := UserData#{rounds := NewRounds}}}}
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

to_user_atom(User) when not is_atom(User) ->
    IgnoreCase = string:casefold(User),
    binary_to_atom(unicode:characters_to_binary(IgnoreCase)).

round_id(#{course:=Name, date:=[Y,M,D]}) ->
    unicode:characters_to_binary(io_lib:format("~ts ~w-~2..0w-~2..0w", [Name,Y,M,D])).

id_to_name_and_date(Str0) ->
    [Str1, Day] = string:split(Str0, "-", trailing),
    [Str2, Month] = string:split(Str1, "-", trailing),
    [Name, Year] = string:split(Str2, " ", trailing),
    {unicode:characters_to_list(Name),
     [binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)]}.

find_round(String, Rounds) ->
    {Name, Date} = id_to_name_and_date(String),
    %% io:format("~p => ~p ~p ~n",[String, Name, Date]),
    Find = fun(#{course := N, date := D}) ->
                   N == Name andalso D == Date
           end,
    lists:search(Find, Rounds).

default_menus(Rounds) ->
    Found = [Year || #{date := [Year|_]} <- Rounds],
    {ThisYear,_,_} = date(),
    Years = lists:reverse(lists:usort([ThisYear|Found])),
    YearsStrings = [integer_to_list(Year) || Year <- Years],
    LastRounds = ["Last " ++ integer_to_list(Rs) || Rs <- [2,3,5,10,20], Rs =< length(Rounds)],
    All = ["All Rounds"] ++ YearsStrings ++ LastRounds,
    [unicode:characters_to_binary(Str) || Str <- All].

split_round_data(String, All) ->
    Year = case string:to_integer(String) of
               {error, _} -> undefined;
               {Int, _} when is_integer(Int), 1970 < Int -> Int;
               _ -> undefined
           end,

    %% Def = length(default_menus(All)),
    case {String, is_integer(Year)} of
        {<<"All Rounds">>, false} ->
            {io_lib:format("All rounds (~w)", [length(All)]), All, []};
        {<<"Last ", Num/binary>>, false} ->
            {N, _} = string:to_integer(Num),
            {L5, Rest} = lists:split(N, All),
            {String, L5, Rest};
        {_, false} ->
            case find_round(String, All) of
                {value, Round} ->
                    #{course:=Course0} = Round,
                    Rest = lists:delete(Round, All),
                    [Course1|_] = string:split(Course0, "Golf"),
                    [Course|_] = string:split(Course1, "golf"),
                    {string:trim(Course), [Round], Rest};
                false ->
                    io:format("Round ~ts not found ~p ~n", [String, id_to_name_and_date(String)]),
                    not_found
            end;
        {_, true} ->
            {YearData, Rest} = lists:partition(fun(#{date := [Y|_]}) -> Y =:= Year end, All),
            {io_lib:format("Year ~s (~w)", [String, length(YearData)]), YearData, Rest}
    end.

diagram_data(Desc, Rounds, Other) ->
    {DiDa,LBs} = split_rounds(Desc, Rounds, Other, Other =/= [] andalso length(Rounds) =/= 1),
    {diagram_data(DiDa), LBs}.

split_rounds(_String, Rounds, [], _) ->
    split_rounds(Rounds, false);
split_rounds(String, Rounds, Other, KeepYear) ->
    {Data, Labels} = split_rounds(Other, KeepYear),
    {Data ++ [Rounds], Labels ++ [String]}.

split_rounds(All, KeepYear) ->
    {CurrentYear, _, _} = erlang:date(),
    CombinedYearData = split_years(CurrentYear, All),
    case CombinedYearData of
        Old when KeepYear ->
            lists:unzip(lists:reverse(Old));
        [{This,_},{Prev,_}|Old] when length(This) < 7 ->
            {Data1, Label1} = split_current(This),
            {Data2, Label2} = split(Prev, 5,0, Data1, Label1),
            {YearData, YearLabels} = lists:unzip(lists:reverse(Old)),
            {YearData ++ Data2, YearLabels ++ Label2};
        [{This, _}|Old] ->
            {Data1, Label1} = split_current(This),
            {YearData, YearLabels} = lists:unzip(lists:reverse(Old)),
            {YearData ++ Data1, YearLabels ++ Label1}
    end.

split_current([]) ->
    {[], []};
split_current([L1]) ->
    {[[L1]], [date_str(L1,1)]};
split_current([L1,L2]) ->
    {[[L2],[L1]], [date_str(L2,1), date_str(L1,1)]};
split_current([L1,L2|Rest]) ->
    split(Rest, 5,0, [[L2],[L1]], [date_str(L2,1), date_str(L1,1)]).

split(List, N, C, Acc, Lbls) when C < 2 ->
    case length(List) > 2*N of
        true ->
            {First, Rest} = lists:split(N, List),
            split(Rest, N, C+1, [First|Acc], [date_str(First,N)|Lbls]);
        false ->
            {[List|Acc], [date_str(List, length(List))|Lbls]}
    end;
split(List, N, _C, Acc, Lbls) ->
    split(List, N*2, 0, Acc, Lbls).

split_years(Current, Rounds) when Current > 1900 ->
    {YearRs,Older} = lists:splitwith(fun(#{date := [Year|_]}) -> Year =:= Current end, Rounds),
    Data = {YearRs, date_str(Current, length(YearRs))},
    case Older of
        [] ->
            [Data];
        _ when YearRs =:= [] ->
            split_years(Current-1, Older);
        _ ->
            [Data|split_years(Current-1, Older)]
    end.

date_str([H|_], N) ->
    date_str(H,N);
date_str(#{date:=[_Y,M,D]}, N) ->
    io_lib:format("~w/~w(~w)", [D,M,N]);
date_str(Year, N) when is_integer(Year) ->
    io_lib:format("~w(~w)", [Year, N]).

diagram_data([]) ->
    D0 = gs_stats:diagram_data([]),
    Init = [[{T,[D]} || {T,D} <- D1] || D1 <- D0],
    merge_data([], Init);
diagram_data([Hd|Rounds]) ->
    D0 = gs_stats:diagram_data(Hd),
    Init = [[{T,[D]} || {T,D} <- D1] || D1 <- D0],
    merge_data(Rounds, Init).

merge_data([R|Rs], DataLists) ->
    RDs = gs_stats:diagram_data(R),
    Acc = [merge_data2(R1, D1) || {R1,D1} <- lists:zip(RDs,DataLists)],
    merge_data(Rs, Acc);
merge_data([], Acc) ->
    [[{T,lists:reverse(D)} || {T,D} <- D1] || D1 <- Acc].

merge_data2([{T,D}|R], [{T,C}|CR]) ->
    [{T,[D|C]} | merge_data2(R, CR)];
merge_data2([], []) ->
    [].

sort_rounds(Rounds) ->
    Order = fun(#{date := A},#{date := B}) -> A >= B end,
    lists:sort(Order, Rounds).


