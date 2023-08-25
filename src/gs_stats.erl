-module(gs_stats).

-export([empty/0, no_shots/1, set_hole_data/3,
         add_hole/3, get_hole/2, set_round_data/3,
         keys/0, key/1, shots/0,
         read/1, save/2,
         read_courses/1, save_courses/2,
         print_stats/3, print_holes/1,
         diagram_data/1
        ]).

read(File) ->
    case filename:extension(File) of
        ".txt" -> read_erlang(File);
        _ -> read_json(File)
    end.

read_erlang(File) ->
    case file:consult(File) of
        {ok, Data} ->
            %% io:format("Prev: ~P~n",[Data, 10]),
            try
                JsonData = to_json(Data),
                FileWOExt = filename:rootname(File),
                FileJson = FileWOExt ++ ".json",
                io:format("Convert to file: ~s~n",[FileJson]),
                ok = file:write_file(FileJson, JsonData)
            catch _:Reason:ST ->
                    io:format("error: ~p~n ~W~n", [Reason, ST, 20])
            end,
            Data;
        {error, enoent} ->
            io:format("Starting new data, File not found: ~s~n",[File]),
            []
    end.

read_json(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Rounds = jsone:decode(Bin, [{keys, atom}]),
            ConvRound = fun(Map) ->
                                All = [{convert_key_from_json(Key), convert_val_from_json(Val)} || {Key,Val} <- maps:to_list(Map)],
                                maps:from_list(All)
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

convert_key_from_json(Key) ->
    case string:to_integer(atom_to_binary(Key)) of
        {Int, <<>>} when is_integer(Int) ->
            Int;
        _ ->
            Key
    end.

convert_val_from_json(Val) when is_map(Val) ->
    Val;
convert_val_from_json(Val) when is_integer(Val) ->
    Val;
convert_val_from_json(Val) when is_binary(Val) ->
    String = unicode:characters_to_list(Val),
    try convert_to_date(String) of
        Date -> Date
    catch _:_ ->
            String
    end.

convert_to_date(Str0) ->
    {Year, [$-|Str1]} = string:to_integer(Str0),
    {Month, [$-|Str2]} = string:to_integer(Str1),
    {Day, _} = string:to_integer(Str2),
    [Year, Month, Day].


to_json(Data) ->
    ConvRound = fun(Map) ->
                        All = [{convert_key_to_json(Key), convert_val_to_json(Val)} || {Key,Val} <- maps:to_list(Map)],
                        maps:from_list(All)
                end,
    PreJson = [ConvRound(R) || R <- Data],
    jsone:encode(PreJson, [native_utf8, {space, 0}, {indent, 1}]).

convert_key_to_json(Int) when is_integer(Int) ->
    integer_to_binary(Int);
convert_key_to_json(Key) ->
    Key.

convert_val_to_json([Y,M,D]) when is_integer(Y), Y > 2000 ->
    {{Y,M,D},{10,0,0}};
convert_val_to_json(String) when is_list(String) ->
    unicode:characters_to_binary(String);
convert_val_to_json(Int) when is_integer(Int) ->
    Int;
convert_val_to_json(Map) when is_map(Map) ->
    Map.

save(File, Data) ->
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

no_shots(Hole) ->
    lists:sum(lists:flatten([maps:values(M) || M <- maps:values(Hole), is_map(M)])).

%% Round
add_hole(HoleNo, Hole, Round) ->
    Round#{HoleNo => Hole}.

get_hole(Hole,Round) ->
    maps:get(Hole, Round, gs_stats:empty()).

set_round_data(Key, Data, Round) ->
    Round#{Key=>Data}.

%% Shot types

key(ShotId) ->
    lists:nth(ShotId, keys()).

keys() ->
    [drive, woods, iron, wedge, pitch, bunker, chip, drop, 'long putt', 'medium putt', 'short putt'].

shots() ->
    [{"Drive",  "Max length shots"},
     {"Woods",  "180-230m Long shots"},
     {"Iron",   "120-180m Iron shots"},
     {"Wedge",  "60-120m Wedge shots"},
     {"Pitch",  "20-60m Pitches"},
     {"Bunkers","Bunker shots close green"},
     {"Chips",  "Chipping"},
     {"Drop", "Penaltys"},
     {"Long putts", "Longer than 3m putts"},
     {"Medium putts", "1-3m putts"},
     {"Short putts", "0-1m putts"}
    ].

%%% Stats

merge_round(Round0) ->
    lists:foldl(fun({No,Hole}, Acc) ->
                        case is_integer(No) of
                            true ->
                                case analyze_hole(Hole) of
                                    ignore -> Acc;
                                    HoleData -> merge(HoleData, Acc)
                                end;
                            false -> Acc
                        end
                end,
                empty(), maps:to_list(Round0)).

merge(Prev, Round0) ->
    Add = fun(_, V1, V2) -> V1+V2 end,
    DoShots = fun(_, S1, S2) when is_map(S1) -> maps:merge_with(Add, S1, S2);
                 (_, S1, S2) when is_integer(S1) -> S1 + S2
              end,
    maps:merge_with(DoShots, Prev, Round0).

analyze_hole(Hole) ->
    {Par, Shots, Putts} = hole_to_shots(Hole),
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
            UpAndDown = bool_to_int(Total =< Par andalso Short > 0 andalso Putts =< 1),
            {P1,P2} = other_par(Par),

            Stat = #{count => 1, {par,Par} => Total, {par_n,Par} => 1,
                     {par, P1} => 0, {par, P2} => 0,  %% Default values prohibit crash later
                     gir=> Gir, 'par save' => ParSave, 'up and down' => UpAndDown,
                     putts => Putts, {putt,PuttsN} => 1,
                     hio => Hio, albatross => Albatross, eagle => Eagle, birdie => Birde, par => MadePar,
                     bogey => Bogey, 'double bogey' => DBogey, 'triple bogey' => TBogey, other => Other
                    },
            Hole#{stat => Stat}
    end.

other_par(3) -> {4,5};
other_par(4) -> {3,5};
other_par(5) -> {3,4}.


bool_to_int(false) -> 0;
bool_to_int(true) -> 1.

hole_to_shots(Hole) ->
    maps:fold(fun sum_shot/3, {undef,0,0}, Hole).

sum_shot('long putt', Data, {Par,Shots,Putts}) ->
    {Par, Shots, sum(Data)+Putts};
sum_shot('medium putt', Data, {Par, Shots,Putts}) ->
    {Par, Shots, sum(Data)+Putts};
sum_shot('short putt', Data, {Par, Shots,Putts}) ->
    {Par, Shots, sum(Data)+Putts};
sum_shot(par, Par, {_Par, Shots,Putts}) ->
    {Par, Shots, Putts};
sum_shot(_, Data, {Par, Shots,Putts}) ->
    {Par, sum(Data)+Shots, Putts}.

sum(#{bad:=Bad,good:=Good,perfect:=Perfect}) ->
    Bad+Good+Perfect.

diagram_data(Rounds) ->
    NoRounds = max(length(Rounds), 1),
    [{par, _Par},{stat, Stat}|ShotStats] = collect(Rounds),

    F = fun(P) ->
                case P of
                    'medium putt'  -> "Med putt";
                    'double bogey' -> "Double";
                    'triple bogey' -> "Triple";
                    'up and down'  -> "Up&Dwn";
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
                      NoHoles = maps:get(count,Stat),
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
    D4 = [{F(Type), maps:get(Type, Stat, 0) / NoRounds} ||
             Type <- [ gir, 'par save', 'up and down' ]],

    Scores = [{io_lib:format("Par ~w", [N]), maps:get({par,N}, Stat, 0)/maps:get({par_n,N},Stat,1)}
              || N <- [3,4,5]],
    [D11, D13, D14 ++ [PuttPercent], D2 ++ [PuttPerHole], D4 ++ [Drop], D3, Scores].

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
     shot_stats(ShotStats, Stat, RestStats),
     putt_stats(Stat, RestStats),
     io_lib:nl(),
     hole_stats(Stat, RestStats),
     io_lib:nl(),
     play_stats(Stat, RestStats),
     io_lib:nl(),
     score_stats(Stat, RestStats),
     io_lib:nl(),
     training(ShotStats),
     io_lib:nl()
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
    Rest = case RestRounds of
               [_,{stat,#{count:=RN}}|RestShots] ->
                   {RN,RestShots};
               false ->
                   false
           end,
    [io_lib:format("~12s:  bad   ok   good  per round  per hole total ~n", ["shoot type"]) |
     [shot_stat(Shot, NoHoles, Rest) || Shot <- Shots]].

shot_stat({drop, #{bad:=Drop}}, NoHoles, Rest) ->
    String = case Rest of
                 false -> format_line(drop, Drop, NoHoles);
                 {RN,RestShots} ->
                     {_, #{bad:=RBad}} = lists:keyfind(drop, 1, RestShots),
                     format_line(drop, Drop, NoHoles, RBad/RN*18, fun lessthan/2)
             end,
    [String, io_lib:nl()];
shot_stat({Key, #{bad:=Bad,good:=Good,perfect:=Perfect}}, NoHoles, Rest) ->
    case {Bad+Good+Perfect, Rest} of
        {0,_} -> io_lib:format("~12s~n", [Key]);
        {Total, false} ->
            io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f ~9.2f ~5w~n",
                          [Key, round(Bad/Total*100), round(Good/Total*100), round(Perfect/Total*100),
                           Total/NoHoles*18, Total/NoHoles, Total]);
        {Total, {_N,List}} when is_list(List) ->
            {_, #{bad:=RBad,good:=RG,perfect:=RP}} = lists:keyfind(Key, 1, List),
            RTotal = RBad+ RG + RP,
            [_, Trend, _] = greathan((Good+Perfect)/Total, (RG+RP)/RTotal),
            io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f ~9.2f ~5w  ~ts~n",
                          [Key, round(Bad/Total*100), round(Good/Total*100), round(Perfect/Total*100),
                           Total/NoHoles*18, Total/NoHoles, Total, Trend])
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
                    io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f ~9.2f ~5w~n~n",
                                  ['putts', round(Bad/PuttHs*100), round(Good/PuttHs*100),
                                   round(Perfect/PuttHs*100), Total/NoHoles*18, Total/NoHoles, Total]);
                [_,{stat,RStat}|_] ->
                    RTotal = maps:get(putts, RStat, 0),
                    RN = maps:get(count, RStat),
                    [PPR|Trend] = lessthan(Total/NoHoles*18,RTotal/RN*18),
                    io_lib:format("~12s ~4w% ~4w% ~4w% ~9.2f ~9.2f ~5w  ~ts (~.2f)~n~n",
                                  ['putts', round(Bad/PuttHs*100), round(Good/PuttHs*100),
                                   round(Perfect/PuttHs*100), PPR, Total/NoHoles, Total|Trend])
            end,
    [Putts| [stat_line(Type, {putt,N}, Stat, Rest) ||
                {Type, N} <- [{'one putt', 1}, {'two putt', 2}, {'three putt',3}, {'putt > 3', n}]]
    ].

hole_stats(Stat, Rest) ->
    [stat_line(Type, Stat, Rest) ||
        Type <- [hio, albatross, eagle, birdie, par, bogey, 'double bogey', 'triple bogey', other]].

play_stats(Stat, Rest) ->
    [stat_line(Type, Stat, Rest) ||
        Type <- [gir, 'par save', 'up and down']].

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
    io_lib:format("~12s ~17c ~9.2f ~9.2f ~5w~n", [Type, $\s, Shots/NoHoles*18, Shots/NoHoles, Shots]).

format_line(Type, Shots, NoHoles, PerRounds, Compare) ->
    PerRound = Shots/NoHoles*18,
    [_| Res] = Compare(PerRound, PerRounds),
    io_lib:format("~12s ~17c ~9.2f ~9.2f ~5w  ~ts (~.2f)~n",
                  [Type, $\s, PerRound, Shots/NoHoles, Shots|Res]).

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
compare({putt, 1}) -> fun greathan/2;
compare({putt, 2}) -> fun greathan/2;
compare({putt, 3}) -> fun lessthan/2;
compare({putt, n}) -> fun lessthan/2;
compare(What) ->
    io:format("~p: NYI compare ~p~n",[?LINE,What]).

greathan(PA, PB) when PA >= PB ->
    [PA, [16#2197], PB];
greathan(PA,PB) ->
    [PA, [16#2198], PB].

lessthan(PA, PB) when PA =< PB ->
    [PA, [16#2197], PB];
lessthan(PA,PB) ->
    [PA, [16#2198], PB].

sort(KeyList) ->
    {SortOrderList,_} = lists:foldl(fun(Key, {Acc,Id}) -> {[{Key, Id}|Acc],Id+1} end, {[], 1}, [par, stat|keys()]),
    SortOrder = maps:from_list(SortOrderList),
    Sorted = lists:sort([{maps:get(Key,SortOrder), {Key,Data}} || {Key,Data} <- KeyList]),
    [KeyData || {_, KeyData} <- Sorted].


print_holes(Stat) ->
    List = lists:sort(maps:to_list(Stat)),
    [print_hole(No, H) || {No, H} <- List, is_integer(No)].

print_hole(No, #{drop:=#{bad:=Drop}} = Hole) ->
    {Par, Shots, Putts} = hole_to_shots(Hole),
    io:format("~.2w ~w ~.2w ~w ~w~n",[No, Par, Shots-Drop, Drop, Putts]).
