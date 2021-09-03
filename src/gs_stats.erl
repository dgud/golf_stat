-module(gs_stats).

-export([empty/0, no_shots/1, set_hole_data/3,
         add_hole/3, get_hole/2, set_round_data/3,
         keys/0, key/1, shots/0,
         read/1, save/2,
         read_courses/1, save_courses/2,
         print_stats/1, print_holes/1
        ]).

read(File) ->
    case file:consult(File) of
        {ok, Data} ->
            %% io:format("Prev: ~p~n",[Data]),
            Data;
        {error, enoent} ->
            io:format("Starting new data, File not found: ~s~n",[File]),
            []
    end.

save(File, Data) ->
    Formatted = [io_lib:format("~p.~n", [Course]) || Course <- Data],
    _ = file:rename(File, File ++ ".old"),
    file:write_file(File, unicode:characters_to_binary(Formatted)).

read_courses(Dir) ->
    {ok, Bin} = file:read_file(filename:join(Dir, "courses.txt")),
    Courses = jsone:decode(Bin, [{keys, atom}]),
    %% io:format("~p~n",[Courses]),
    Prefixed = [{Name,Course} ||  #{name:=Name} = Course <- Courses],
    [Course || {_, Course} <- lists:sort(Prefixed)].

save_courses(Courses, Dir) ->
    Encoded = jsone:encode(Courses, [native_utf8, {space, 0}, {indent, 1}]),
    ok = file:write_file(filename:join(Dir,"courses.txt"), Encoded).

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

print_stats([]) ->
    ignore;
print_stats([#{course:=Course0, date:=Date}]=Stat) ->
    Course = io_lib:format("~ts Date: ~w-~2..0w-~2..0w", [Course0|Date]),
    print_stats(Course, Stat);
print_stats(Stats) ->
    print_stats("Summary of all rounds", Stats).

print_stats(What, Rounds0) ->
    %% io:format("~p: ~p~n~n",[?LINE,Merged]),
    [{par, Par},{stat, Stat0}|ShotStats] = collect(Rounds0),
    NoRounds = length(Rounds0),
    Stat = Stat0#{no_rounds => NoRounds},
    #{count := NoHoles, {par,3} := P3, {par,4} := P4, {par,5} := P5} = Stat,
    NoShots = P3+P4+P5,
    AveragePerRound = (NoShots - Par) / NoHoles * 18.0,
    [What,
     io_lib:format(" Par: ~w Shots: ~w +/- ~w shots ~4.1f per 18 holes~n",
                   [Par, NoShots, NoShots - Par, AveragePerRound]),
     shot_stats(ShotStats, Stat),
     putt_stats(Stat),
     io_lib:nl(),
     hole_stats(Stat),
     io_lib:nl(),
     play_stats(Stat),
     io_lib:nl(),
     score_stats(Stat),
     io_lib:nl(),
     training(ShotStats),
     io_lib:nl()].

collect(Rounds) ->
    Merged = lists:foldl(fun(R, Acc) ->
                                 merge(merge_round(R), Acc)
                         end, empty(), Rounds),
    %% io:format("~p: ~p~n~n",[?LINE,Merged]),
    sort(maps:to_list(Merged)).

shot_stats(Shots, #{count := NoHoles, no_rounds := NoRounds}) ->
    [io_lib:format("~15s:   bad    ok    good    per round   per hole  total ~n", ["shoot type"]) |
     [shot_stat(Shot, NoHoles, NoRounds) || Shot <- Shots]].

shot_stat({drop, #{bad:=Drop}}, NoHoles, NoRounds) ->
    [format_line(drop, Drop, NoHoles, NoRounds),
     io_lib:nl()];
shot_stat({Key, #{bad:=Bad,good:=Good,perfect:=Perfect}}, NoHoles, NoRounds) ->
    case Bad+Good+Perfect of
        0 -> io_lib:format("~15s~n", [Key]);
        Total ->
            io_lib:format("~15s ~5w% ~5w% ~5w%  ~10.2f ~10.2f ~5w~n",
                      [Key, round(Bad/Total*100), round(Good/Total*100), round(Perfect/Total*100),
                       Total/NoRounds, Total/NoHoles, Total])
    end.

format_line(Type, Shots, NoHoles, NoRounds) ->
    io_lib:format("~15s ~21c ~10.2f ~10.2f ~5w~n", [Type, $\s, Shots/NoRounds, Shots/NoHoles, Shots]).

putt_stats(#{count := NoHoles, no_rounds := NoRounds} = Stat) ->
    [format_line('putts', maps:get(putts, Stat, 0), NoHoles, NoRounds)|
     [format_line(Type, maps:get({putt,N}, Stat, 0), NoHoles, NoRounds) ||
         {Type, N} <- [{'one putt', 1}, {'two putt', 2}, {'three putt',3}, {'putt > 3', n}]]].

hole_stats(#{count := NoHoles, no_rounds := NoRounds} = Stat) ->
    [format_line(Type, maps:get(Type, Stat, 0), NoHoles, NoRounds) ||
        Type <- [hio, albatross, eagle, birdie, par, bogey, 'double bogey', 'triple bogey', other]].

play_stats(#{count := NoHoles, no_rounds := NoRounds} = Stat) ->
    [format_line(Type, maps:get(Type, Stat, 0), NoHoles, NoRounds) ||
        Type <- [gir, 'par save', 'up and down']].

score_stats(Stat) ->
    [io_lib:format("Average scores:~n",[])|
     [io_lib:format(" Par ~w: ~5.2f~n", [N, maps:get({par,N}, Stat)/maps:get({par_n,N},Stat,1)]) || N <- [3,4,5]]].

training(Shots) ->
    Rate = fun(Bad, _Good, _Perfect) ->
                   %% Total = Bad+Good+Perfect,
                   %% Succes = (Good+Perfect)/Total * Total/NoShots,
                   Bad
           end,
    Rated = [{Rate(Bad,Good,Perfect), What} || {What, #{bad:=Bad,good:=Good,perfect:=Perfect}} <- Shots],
    Sorted = [What || {_, What} <- lists:reverse(lists:sort(Rated)), What =/= drop],
    io_lib:format("Training order: ~w~n", [Sorted]).


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
