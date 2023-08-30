-module(gs_stats).

-export([empty/0, set_hole_data/3,
         add_hole/3, get_hole/2, set_round_data/3,
         keys/0, key/1, shots/0,
         read_player/1, save_player/2,
         read_courses/1, save_courses/2,
         print_stats/3,
         diagram_data/1
        ]).

read_player(File) ->
    read_json(File).

read_json(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Rounds = jsone:decode(Bin, [{keys, atom}]),
            ConvRound = fun(Round) ->
                                All = convert_map_from_json(Round),
                                case All of
                                    #{holes := _} -> %% New format
                                        All;
                                    _ -> %% Old format
                                        try convert_old(maps:to_list(All))
                                        catch _:Reason:ST ->
                                                io:format("~p: Failed: ~p ~p~n",[?LINE, Reason,ST]),
                                                io:format(" ~p ~p~n",[maps:get(date, Round), maps:get(course, Round)]),
                                                exit(fail)
                                        end
                                end
                        end,
            [ConvRound(R) || R <- Rounds];
        {error, _} ->
            show_error(io_lib:format("Starting new data, File not found: ~s~n", [File])),
            []
    end.

%% show_error(enoent, What) ->
%%     show_error(io_lib:format("Could not find file: ~p~n",[What])).

show_error(String) when is_list(String) ->
    io:put_chars(user, String),
    MD = wxMessageDialog:new(wx:new(), String),
    wxMessageDialog:showModal(MD),
    wxMessageDialog:destroy(MD).

convert_map_from_json(Map) when is_map(Map) ->
    All = [{convert_key_from_json(Key), convert_val_from_json(Key, Val)} || {Key,Val} <- maps:to_list(Map)],
    maps:from_list(All).

convert_key_from_json(Key) ->  %% Old format
    case string:to_integer(atom_to_binary(Key)) of
        {Int, <<>>} when is_integer(Int) ->
            Int;
        _ ->
            Key
    end.

convert_val_from_json(_, Val) when is_map(Val) ->
    convert_map_from_json(Val);
convert_val_from_json(_, Val) when is_integer(Val) ->
    Val;
convert_val_from_json(club, Val) when is_binary(Val) ->
    binary_to_atom(Val);
convert_val_from_json(_, Val) when is_binary(Val) ->
    String = unicode:characters_to_list(Val),
    try convert_to_date(String) of
        Date -> Date
    catch _:_ ->
            String
    end;
convert_val_from_json(_, List) when is_list(List) ->
    [convert_val_from_json(undefined, E) || E <- List].

convert_to_date(Str0) ->
    {Year, [$-|Str1]} = string:to_integer(Str0),
    {Month, [$-|Str2]} = string:to_integer(Str1),
    {Day, _} = string:to_integer(Str2),
    [Year, Month, Day].


to_json(Data) ->
    PreJson = [conv_map_to_json(R) || R <- Data],
    jsone:encode(PreJson, [native_utf8, {space, 0}, {indent, 1}]).

conv_map_to_json(Map) ->
    All = [{convert_key_to_json(Key), convert_val_to_json(Val)} || {Key,Val} <- maps:to_list(Map)],
    maps:from_list(All).

convert_key_to_json(Int) when is_integer(Int) ->  %% Old format
    integer_to_binary(Int);
convert_key_to_json(Key) when is_atom(Key) ->
    Key.

convert_val_to_json([Y,M,D]) when is_integer(Y), Y > 2000 ->
    {{Y,M,D},{10,0,0}};
convert_val_to_json(String) when is_list(String), is_integer(hd(String)) ->
    unicode:characters_to_binary(String);
convert_val_to_json(List) when is_list(List) ->
    [convert_val_to_json(E) || E <- List];
convert_val_to_json(Int) when is_integer(Int) ->
    Int;
convert_val_to_json(Map) when is_map(Map) ->
    conv_map_to_json(Map);
convert_val_to_json(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom).

save_player(File, Data) ->
    _ = file:rename(File, File ++ ".old"),
    Json = to_json(Data),
    file:write_file(File, Json).

read_courses(Dir) ->
    File = filename:join(Dir, "courses.json"),
    case file:read_file(File) of
        {ok, Bin} ->
            Courses = jsone:decode(Bin, [{keys, atom}]),
            %% io:format("~tp~n",[Courses]),
            Prefixed = [{Name,Course} ||  #{name:=Name} = Course <- Courses],
            [Course || {_, Course} <- lists:sort(Prefixed)];
        {error, enoent} ->
            io:format("Could not find \"courses.json\" file~n Looking at: ~s~n",[File]),
            error({error, "courses.json not found"})
    end.

save_courses(Courses, Dir) ->
    Encoded = jsone:encode(Courses, [native_utf8, {space, 0}, {indent, 1}]),
    ok = file:write_file(filename:join(Dir,"courses.json"), Encoded).

%% Hole
empty() ->
    maps:from_list([{Key, #{bad=>0, good=>0, perfect=>0}} || Key <- keys()]).

set_hole_data(Key, Data, Hole) ->
    Hole#{Key => Data}.

%% Round
add_hole(HoleNo, Hole, Round) ->
    Holes0  = maps:get(holes, Round, []),
    Holes1 = case lists:search(fun(#{no := No}) -> No == HoleNo end, Holes0) of
                 false -> Holes0;
                 {value, OldHole} -> lists:delete(OldHole, Holes0)
             end,
    Holes = lists:sort(fun(#{no := No1}, #{no := No2}) -> No1 =< No2 end,
                       [Hole|Holes1]),
    Round#{holes => Holes}.

get_hole(HoleNo, #{holes := Holes}) ->
    case lists:search(fun(#{no := No}) -> No == HoleNo end, Holes) of
        false -> #{no => HoleNo, shots => []};
        {value, Hole} -> Hole
    end.

set_round_data(Key, Data, Round) ->
    Round#{Key=>Data}.

%% Shot types

key(ShotId) ->
    lists:nth(ShotId, keys()).

keys() ->
    [drive, woods, iron, wedge, pitch, bunker, chip, drop, 'long putt', 'medium putt', 'short putt'].

shots() ->
    [{"Drive",  "Driver shots"},
     {"Woods",  "All other wood shots (180-220m)"},
     {"Iron",   "Iron shots (110-180m)"},
     {"Wedge",  "Full Wedge shots (80-110m)"},
     {"Pitch",  "20-60m Pitches"},
     {"Bunkers","Bunker shots close green"},
     {"Chips",  "Chipping"},
     {"Drop", "Penaltys"},
     {"Long putts", "Longer than 3m putts"},
     {"Medium putts", "1-3m putts"},
     {"Short putts", "0-1m putts"}
    ].

%%% Stats

merge_round(#{holes:=Holes}) ->
    lists:foldl(fun(Hole, Acc) ->
                        case analyze_hole(Hole) of
                            ignore -> Acc;
                            HoleData -> merge(HoleData, Acc)
                        end
                end,
                empty(), Holes).

merge(Prev, Round0) ->
    Add = fun(_, V1, V2) -> V1+V2 end,
    DoShots = fun(_, S1, S2) when is_map(S1) -> maps:merge_with(Add, S1, S2);
                 (_, S1, S2) when is_integer(S1) -> S1 + S2
              end,
    maps:merge_with(DoShots, Prev, Round0).

analyze_hole(#{par:=Par, shots:=AllShots}) ->
    {Shots, Putts} = count_shots(AllShots),
    Hole = lists:foldl(fun expand_shots/2, empty(), AllShots),
    case Shots of
        0 -> ignore;
        _ ->
            Total = Shots + Putts,
            PuttsN = if Putts > 3 -> n;
                        true -> Putts
                     end,
            Hio = bool_to_int(Total =:= 1),
            Albatross = bool_to_int(Total =:= (Par-3)),
            Eagle = bool_to_int(Total =:= (Par-2)),
            Birde = bool_to_int(Total =:= (Par-1)),
            MadePar = bool_to_int(Total =:= Par),
            Bogey = bool_to_int(Total =:= (Par+1)),
            DBogey = bool_to_int(Total =:= (Par+2)),
            TBogey = bool_to_int(Total =:= (Par+3)),
            Other  = bool_to_int(Total > (Par+3)),

            Gir0  = Shots =< (Par - 2),
            Gir   = bool_to_int(Gir0),
            ParSave = bool_to_int(Putts =< 1 andalso Total == Par),
            #{chip := Chip, bunker := Bunker, pitch := Pitch} = Hole,
            Short = sum(Chip)+sum(Bunker)+sum(Pitch),
            UpAndDown = bool_to_int(Short > 0 andalso Putts =< 1),
            {P1,P2} = other_par(Par),

            Stat = #{count => 1, {par,Par} => Total, {par_n,Par} => 1,
                     {par, P1} => 0, {par, P2} => 0,  %% Default values prohibit crash later
                     gir=> Gir, 'par save' => ParSave, 'up and down' => UpAndDown,
                     putts => Putts, {putt,PuttsN} => 1,
                     hio => Hio, albatross => Albatross, eagle => Eagle, birdie => Birde, par => MadePar,
                     bogey => Bogey, 'double bogey' => DBogey, 'triple bogey' => TBogey, other => Other
                    },
            Hole#{par=> Par, stat => Stat}
    end.

other_par(3) -> {4,5};
other_par(4) -> {3,5};
other_par(5) -> {3,4}.

bool_to_int(false) -> 0;
bool_to_int(true) -> 1.

count_shots(Shots) ->
    lists:foldl(fun sum_shot/2, {0,0}, Shots).

sum_shot(#{club:='long putt'} = Shot, {Shots,Putts}) ->
    {Shots, sum(Shot)+Putts};
sum_shot(#{club:='medium putt'} = Shot, {Shots,Putts}) ->
    {Shots, sum(Shot)+Putts};
sum_shot(#{club:='short putt'} = Shot, {Shots,Putts}) ->
    {Shots, sum(Shot)+Putts};
sum_shot(#{club:=_} = Shot, {Shots,Putts}) ->
    {sum(Shot)+Shots, Putts}.

sum(Shot) ->
    maps:get(bad, Shot, 0) + maps:get(good, Shot, 0) + maps:get(perfect, Shot, 0).

expand_shots(#{club:=Club} = Shot, Acc) ->
    #{Club := Count} = Acc,
    Acc#{Club := merge(maps:remove(club, Shot), Count)}.


diagram_data(Rounds) ->
    %% NoRounds = max(length(Rounds), 1),
    [{par, _Par},{stat, #{count := NoHoles0} = Stat}|ShotStats] = collect(Rounds),
    NoHoles = max(1, NoHoles0), %% avoid division by zero
    NoRounds = max(1, NoHoles0/18),

    F = fun(P) ->
                case P of
                    'medium putt'  -> "Med putt";
                    'double bogey' -> "Double";
                    'triple bogey' -> "Triple";
                    'up and down'  -> "Up&Dwn";
                    putts -> "Putts/Hole";
                    _ -> io_lib:format("~s", [P])
                end
        end,
    Shots = fun({drop, #{bad:=Bad}}) ->
                    {"drop", Bad / NoRounds};
               ({Key, #{bad:=Bad,good:=Good,perfect:=Perfect}}) ->
                    Total = (Bad+Good+Perfect),
                    case Total of
                        0 -> {F(Key), 100.0};
                        _ -> {F(Key), 100*(Good+Perfect)/Total}
                    end
            end,
    D1 = lists:map(Shots, ShotStats),
    {D11,D12} = lists:splitwith(fun({K,_}) -> "pitch" =/= K end,
                                D1),
    {D13,[Drop|D14]} = lists:splitwith(fun({K,_}) -> "drop" =/= K end,
                                       D12),
    Putting = fun() ->
                      Putts = maps:get(putts, Stat),
                      Bad = maps:get({putt,3}, Stat, 0) + maps:get({putt,n}, Stat, 0),
                      Good = maps:get({putt,2}, Stat, 0),
                      Perfect = maps:get({putt,1}, Stat, 0),
                      Total = (Bad+Good+Perfect),
                      case Total of
                          0 -> [{F('putts'), 100.0}, {F('Putts/Hole'), 0}];
                          _ -> [{F('putts'), 100*(Good+Perfect)/Total}, {F('Putts/Hole'), Putts/NoHoles}]
                      end
              end,
    [PuttPercent, PuttPerHole] = Putting(),

    D2 = [{F(Type), maps:get({putt, N}, Stat, 0) / NoRounds} ||
             {Type, N} <-  [{'1 putt', 1}, {'2 putt', 2}, {'3 putt',3}, {'putt > 3', n}]],
    D3 = [{F(Type), maps:get(Type, Stat, 0) / NoRounds} ||
             Type <- [%%hio, albatross, eagle,
                      birdie, par,
                      bogey, 'double bogey', 'triple bogey', other
                     ]],
    D4 = [{F(Type), maps:get(Type, Stat, 0) / Div} ||
             {Type, Div} <- [{gir, NoRounds}, {'up and down', NoRounds},
                             {putts, NoHoles}, {'par save', NoRounds}]],

    Scores = [{io_lib:format("Par ~w", [N]), maps:get({par,N}, Stat, 0)/maps:get({par_n,N},Stat,1)}
              || N <- [3,4,5]],
    [D4 ++ [Drop], Scores, D11, D13, D14 ++ [PuttPercent], D2 ++ [PuttPerHole], D3].

print_stats(_, [], _) ->
    "No stats available";
print_stats(_, [#{course:=Course0, date:=Date}]=Stat, Rest) ->
    Course = io_lib:format("~ts Date: ~w-~2..0w-~2..0w", [Course0|Date]),
    print_stats_1(Course, Stat, Rest);
print_stats(Header, Stats, Rest) ->
    print_stats_1(Header, Stats, Rest).

print_stats_1(What, Rounds0, Rest) ->
    %% io:format("~p: ~p~n~n",[?LINE,Merged]),
    [{par, Par},{stat, Stat}|ShotStats] = collect(Rounds0),
    RestStats = Rest /= [] andalso collect(Rest),

    #{count := NoHoles, {par,3} := P3, {par,4} := P4, {par,5} := P5} = Stat,
    NoShots = P3+P4+P5,
    AveragePerRound = (NoShots - Par) / NoHoles * 18.0,
    [What,"\n",
     io_lib:format(" Par: ~w Shots: ~w +/- ~w shots ~4.1f per 18 holes~n",
                   [Par, NoShots, NoShots - Par, AveragePerRound]),
     io_lib:nl(),
     play_stats(Stat, ShotStats, RestStats),
     io_lib:nl(),
     score_stats(Stat, RestStats),
     io_lib:nl(),
     shot_stats(ShotStats, Stat, RestStats),
     putt_stats(Stat, RestStats),
     io_lib:nl(),
     hole_stats(Stat, RestStats),
     io_lib:nl(),
     training(ShotStats)
    ].

collect([]) ->
    Stat = #{count => 0, {par,3} => 0, {par_n,4} => 1,
             {par, 4} => 0, {par, 5} => 0,
             gir=> 0, 'par save' => 0, 'up and down' => 0,
             putts => 0, {putt,0} => 1,
             hio => 0, albatross => 0, eagle => 0, birdie => 0, par => 0,
             bogey => 0, 'double bogey' => 0, 'triple bogey' => 0, other => 0,
             no_rounds => 0
            },
    Empty = merge(#{stat => Stat, par => 4}, empty()),
    sort(maps:to_list(Empty));
collect(Rounds) ->
    Merged = #{stat := Stat} =
        lists:foldl(fun(R, Acc) ->
                            merge(merge_round(R), Acc)
                    end, empty(), Rounds),
    %% io:format("~p: ~p~n~n",[?LINE,Merged]),
    sort(maps:to_list(Merged#{stat := Stat#{no_rounds => length(Rounds)}})).

shot_stats(Shots, #{count := NoHoles}, RestRounds) ->
    {Rest, TrendDesc} = case RestRounds of
                            [_,{stat,#{count:=RN}}|RestShots] ->
                                {{RN,RestShots}, "Trend"};
                            false ->
                                {false, ""}
           end,
    [io_lib:format("~12s:  bad   ok   good  per round  ~s~n", ["shoot type", TrendDesc]) |
     [shot_stat(Shot, NoHoles, Rest) || Shot <- Shots]].

shot_stat({drop, _}, _NoHoles, _Rest) ->
    [];
shot_stat({Key, #{bad:=Bad,good:=Good,perfect:=Perfect}}, NoHoles, Rest) ->
    case {Bad+Good+Perfect, Rest} of
        {0,_} -> io_lib:format("~12s~n", [Key]);
        {Total, false} ->
            io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f~n",
                          [Key, round(Bad/Total*100), round(Good/Total*100), round(Perfect/Total*100),
                           Total/NoHoles*18]);
        {Total, {_N,List}} when is_list(List) ->
            {_, #{bad:=RBad,good:=RG,perfect:=RP}} = lists:keyfind(Key, 1, List),
            RTotal = RBad+ RG + RP,
            [_, Trend, _] = greathan((Good+Perfect)/Total, percent(RG+RP, RTotal)),
            io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f   ~ts~n",
                          [Key, round(Bad/Total*100), round(Good/Total*100), round(Perfect/Total*100),
                           Total/NoHoles*18, Trend])
    end.


putt_stats(#{count := NoHoles} = Stat, Rest) ->
%%    [format_line('putts', maps:get(putts, Stat, 0), NoHoles, NoRounds), "\n"
    Bad = maps:get({putt,3}, Stat, 0) + maps:get({putt,n}, Stat, 0),
    Good = maps:get({putt,2}, Stat, 0),
    Perfect = maps:get({putt,1}, Stat, 0),
    Total = maps:get(putts, Stat, 0),
    PuttHs = Bad+Good+Perfect,
    Putts = case Rest of
                false ->
                    io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f~n~n",
                                  ['putts', round(Bad/PuttHs*100), round(Good/PuttHs*100),
                                   round(Perfect/PuttHs*100), Total/NoHoles*18]);
                [_,{stat,RStat}|_] ->
                    RTotal = maps:get(putts, RStat, 0),
                    RN = maps:get(count, RStat),
                    [PPR|Trend] = lessthan(Total/NoHoles*18,RTotal/RN*18),
                    io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f   ~ts (~.2f)~n~n",
                                  ['putts', round(Bad/PuttHs*100), round(Good/PuttHs*100),
                                   round(Perfect/PuttHs*100), PPR|Trend])
            end,
    [Putts| [stat_line(Type, {putt,N}, Stat, Rest) ||
                {Type, N} <- [{'one putt', 1}, {'two putt', 2}, {'three putt',3}, {'putt > 3', n}]]
    ].

hole_stats(Stat, Rest) ->
    [stat_line(Type, Stat, Rest) ||
        Type <- [hio, albatross, eagle, birdie, par, bogey, 'double bogey', 'triple bogey', other]].

play_stats(#{count:=NoHoles} = Stat, Shots, Rest) ->
    {_, #{bad:=Drop}} = lists:keyfind(drop, 1, Shots),
    {DropString, TrendDesc}
        = case Rest of
              false ->
                  {format_line(drop, Drop, NoHoles), []};
              [_, {stat, #{count := RN}}|RestShots] ->
                  {_, #{bad:=RBad}} = lists:keyfind(drop, 1, RestShots),
                  {format_line(drop, Drop, NoHoles, RBad/RN*18, fun lessthan/2),
                   "Trend"}
          end,
    [io_lib:format("~12s:                   per round  ~s~n", ["Statistics", TrendDesc]),
     [stat_line(Type, Stat, Rest) || Type <- [gir, 'up and down', putts, 'par save']],
     DropString
    ].

score_stats(Stat, false) ->
    [io_lib:format("Average scores:~n",[])|
     [io_lib:format(" Par ~w: ~5.2f~n", [N, maps:get({par,N}, Stat)/maps:get({par_n,N},Stat,1)]) 
      || N <- [3,4,5]]];
score_stats(Stat, [_, {stat, Rest}|_]) ->
    [io_lib:format("Average scores: (all)~n", [])|
     [io_lib:format(" Par ~w: ~5.2f ~ts (~.2f)~n",
                    [N|lessthan(
                         maps:get({par,N}, Stat)/maps:get({par_n,N},Stat,1),
                         maps:get({par,N}, Rest)/maps:get({par_n,N},Rest,1))
                    ])
      || N <- [3,4,5]]].

stat_line(Type, Stat, Rest) ->
    stat_line(Type, Type, Stat, Rest).

stat_line(Type, Key, #{count := N} = Stat, false) ->
    format_line(Type, maps:get(Key, Stat, 0), N);
stat_line(Type, Key, #{count := N} = Stat, [_,{stat,#{count := Nr} = Rest}|_]) ->
    Old = maps:get(Key, Rest, 0),
    format_line(Type, maps:get(Key, Stat, 0), N, Old/Nr*18, compare(Key)).

format_line(Type, Shots, NoHoles) ->
    io_lib:format("~12s ~17c ~9.2f~n", [Type, $\s, Shots/NoHoles*18]).

format_line(Type, Shots, NoHoles, PerRounds, Compare) ->
    PerRound = Shots/NoHoles*18,
    [_| Res] = Compare(PerRound, PerRounds),
    io_lib:format("~12s ~17c ~9.2f   ~ts (~.2f)~n", [Type, $\s, PerRound|Res]).

training(Shots) ->
    Rate = fun(Bad, _Good, _Perfect) ->
                   %% Total = Bad+Good+Perfect,
                   %% Succes = (Good+Perfect)/Total * Total/NoShots,
                   Bad
           end,
    Rated = [{Rate(Bad,Good,Perfect), What} || {What, #{bad:=Bad,good:=Good,perfect:=Perfect}} <- Shots],
    Sorted = [atom_to_list(What) || {_, What} <- lists:reverse(lists:sort(Rated)), What =/= drop],
    io_lib:format("Training order: ~s~n", [lists:join(" ", Sorted)]).


compare(gir) -> fun greathan/2;
compare('par save') -> fun greathan/2;
compare('up and down') ->  fun greathan/2;
compare(hio) ->  fun greathan/2;
compare(albatross) ->  fun greathan/2;
compare(eagle) ->  fun greathan/2;
compare(birdie) ->  fun greathan/2;
compare(par) ->  fun greathan/2;
compare(bogey) -> fun lessthan/2;
compare('double bogey') -> fun lessthan/2;
compare('triple bogey') -> fun lessthan/2;
compare(other) -> fun lessthan/2;
compare(putts) -> fun lessthan/2;
compare({putt, 1}) -> fun greathan/2;
compare({putt, 2}) -> fun greathan/2;
compare({putt, 3}) -> fun lessthan/2;
compare({putt, n}) -> fun lessthan/2.

greathan(PA, PB) when PA >= PB ->
    [PA, [16#2197], PB];
greathan(PA,PB) ->
    [PA, [16#2198], PB].

lessthan(PA, PB) when PA =< PB ->
    [PA, [16#2197], PB];
lessthan(PA,PB) ->
    [PA, [16#2198], PB].

percent(_Score, Zero) when Zero == 0 ->
    1.0;
percent(Score, Total) ->
    Score/Total.


sort(KeyList) ->
    {SortOrderList,_} = lists:foldl(fun(Key, {Acc,Id}) -> {[{Key, Id}|Acc],Id+1} end, {[], 1}, [par, stat|keys()]),
    SortOrder = maps:from_list(SortOrderList),
    Sorted = lists:sort([{maps:get(Key,SortOrder), {Key,Data}} || {Key,Data} <- KeyList]),
    [KeyData || {_, KeyData} <- Sorted].


convert_old(OldData) ->
    Round = convert_old(lists:keysort(1, OldData), [], []),
    %% io:format("~p: ~p~n",[?LINE, Round]),
    Round.

convert_old([{HoleNo,Shots}|R], Keep, Holes) when is_integer(HoleNo) ->
    Hole0 = convert_hole(Shots),
    Hole = Hole0#{no => HoleNo},
    convert_old(R, Keep, [Hole|Holes]);
convert_old([Data|R], Keep, Holes) ->
    convert_old(R, [Data|Keep], Holes);
convert_old([], Keep, Holes) ->
    maps:from_list([{holes, lists:reverse(Holes)}|Keep]).

convert_hole(#{par := Par, drop := #{bad := Drop}} = Map) ->
    Shots = convert_shots(sort(maps:to_list(Map)), Drop, 1, []),
    #{par => Par, shots => Shots}.

convert_shots([{drop, _}|R], Drop, N, Shots) ->
    convert_shots(R, Drop, N, Shots);
convert_shots([{_Club, #{bad:=0, good:=0, perfect:=0}}|R], Drop, N, Shots) ->
    convert_shots(R, Drop, N, Shots);
convert_shots([{Club, #{bad:=Bad}=Map}|R], Drop, N, Shots) when Bad > 0, Drop > 0 ->
    Shot1 = #{club => Club, bad => 1},
    Shot2 = #{club => drop, bad => 1},
    convert_shots([{Club, Map#{bad:=Bad-1}}|R], Drop-1, N+2, [Shot2,Shot1|Shots]);
convert_shots([{Club, #{bad:=Res}=Map}|R], Drop, N, Shots) when Res > 0 ->
    Shot1 = #{club => Club, bad => 1},
    convert_shots([{Club, Map#{bad:=Res-1}}|R], Drop, N+1, [Shot1|Shots]);
convert_shots([{Club, #{good:=Res}=Map}|R], Drop, N, Shots) when Res > 0 ->
    Shot1 = #{club => Club, good => 1},
    convert_shots([{Club, Map#{good:=Res-1}}|R], Drop, N+1, [Shot1|Shots]);
convert_shots([{Club, #{perfect:=Res}=Map}|R], Drop, N, Shots) when Res > 0 ->
    Shot1 = #{club => Club, perfect => 1},
    convert_shots([{Club, Map#{perfect:=Res-1}}|R], Drop, N+1, [Shot1|Shots]);
convert_shots([], 0, _, Shots) ->
    lists:reverse(Shots);
convert_shots([{par, _}|R], Drop, N, Shots) ->
    convert_shots(R, Drop, N, Shots);
convert_shots([], Drop, _N, Shots) when Drop > 0 ->
    %% Hmm when I have picked up the ball I have added random drop shots, sigh.
    PuttKeys = ['short putt', 'medium putt', 'long putt', drop],
    {Putts, Other} = lists:partition(fun(#{club := Club}) -> lists:member(Club, PuttKeys) end, Shots),
    lists:reverse(Putts  ++ [#{club => drop, bad => 1} || _ <- lists:seq(1,Drop)] ++ Other);
convert_shots(What, Drop, N, Shots) ->
    io:format("~p:~p: ~p ~p ~p ~n",[?MODULE,?LINE, N, Drop, What]),
    io:format(" ~p ~n", [Shots]),
    exit({nyi, What}).



