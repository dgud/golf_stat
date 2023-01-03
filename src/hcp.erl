-module(hcp).
-export([calc/1, dan/0]).

-record(hcp, {date, club, holes, points, hcp}).

calc(Data) ->
    ok = io:setopts(standard_io, [{encoding,unicode}]),
    AllRounds = parse(Data),
    NoRounds = length(AllRounds),
    Rounds = case NoRounds >= 20 of
                 true -> element(1, lists:split(20, AllRounds));
                 false -> AllRounds
             end,
    %% io:format("~p~n",[Rounds]),
    Top8 = top8(Rounds),
    %% io:format("Top 8~n ~p~n",[Top8]),
    {Hcp,WorstCount} = average(Top8, NoRounds),
    [Last|Rest] = lists:reverse(Rounds),

    RoundsTxt = print_rounds(Rounds, Top8),
    Header = io_lib:format("~s~nHcp: ~.1f~n",[RoundsTxt, Hcp]),
    case lists:member(Last, Top8) of
        true when NoRounds >= 20 ->
            RTop8 = top8(Rest),
            {NewHcp,NextWorst} = average(RTop8, NoRounds),
            [Header,
             io_lib:format("Decrease hcp if better than ~.1f ~n", [score(Last)]),
             io_lib:format("Increase hcp if worse than ~.1f ~n", [score(Last)]),
             io_lib:format("Increase hcp to max ~.1f if result is worse than ~.1f~n", [NewHcp, score(NextWorst)]),
             io_lib:format("  New hcp if between: ~.1f - ~.1f~n",[Hcp, NewHcp])];
        false when NoRounds >= 20 ->
            [Header,
             io_lib:format("Decrease hcp if better than ~.1f~n", [score(WorstCount)]),
             io_lib:format("Hcp will not increase~n", [])];
        _ ->
            {NewHcp, NextWorst} = average(Top8, NoRounds+1),
            [Header,
             io_lib:format("Decrease hcp if better than: ~.1f~n", [score(NextWorst)]),
             case round(Hcp*10) div 10 =:= round(NewHcp*10) div 10 of
                 true -> io_lib:format("No hcp increase ~n", []);
                 false -> io_lib:format("if worse increase to: ~.1f~n", [NewHcp])
             end]
    end.

average(Top8, N) when N >= 20 -> average_1(8, Top8);
average(Top8, 19) -> average_1(7, Top8);
average(Top8, 18) -> average_1(6, Top8);
average(Top8, 17) -> average_1(6, Top8);
average(Top8, 16) -> average_1(5, Top8);
average(Top8, 15) -> average_1(5, Top8);
average(Top8, 14) -> average_1(4, Top8);
average(Top8, 13) -> average_1(4, Top8);
average(Top8, 12) -> average_1(4, Top8);
average(Top8, 11) -> average_1(3, Top8);
average(Top8, 10) -> average_1(3, Top8);
average(Top8, 09) -> average_1(3, Top8);
average(Top8, 08) -> average_1(2, Top8);
average(Top8, 07) -> average_1(2, Top8);
average(Top8, 06) -> average_1(2, Top8)-1.0;
average(Top8, 05) -> average_1(1, Top8);
average(Top8, 04) -> average_1(1, Top8)-1.0;
average(Top8,  _) -> average_1(1, Top8)-2.0.

average_1(N, Top8) ->
    {Top, _} = lists:split(N, lists:reverse(Top8)),
    {lists:sum([score(S) || S <- Top]) / N, lists:last(Top)}.

top8(Hcp) ->
    Sorted = sort_hcp(Hcp),
    case length(Sorted) of
        N when N >= 8 ->
            lists:reverse(element(1, lists:split(8,Sorted)));
        _ ->
            lists:reverse(Sorted)
    end.

sort_hcp(List) ->
    lists:keysort(#hcp.hcp, List).

score(Result) ->
    Result#hcp.hcp.

print_rounds(Rounds, Top) ->
    Acc = [io_lib:format("Top Rounds (of ~w) are: ~n",[length(Rounds)])],
    lists:reverse(print_round(Rounds, true, Top, Acc)).

print_round([R|Rest], NewLine, Top, Acc0) ->
    case lists:member(R, Top) of
        true ->
            Acc1 = add(not NewLine, io_lib:nl(), Acc0),
            Acc2 = [io_lib:format("~-14ts ~-30ts ~4.1f~n", [to_string(R#hcp.date), R#hcp.club, R#hcp.hcp])|Acc1],
            Acc3 = add(Rest == [], io_lib:format("  The last round counted as top 8 and will be removed~n",[]), Acc2),
            print_round(Rest, true, Top, Acc3);
        false ->
            Acc1 = add(NewLine, io_lib:format(" Skipped:",[]), Acc0),
            Acc2 = [io_lib:format(" ~4.1f",[R#hcp.hcp])|Acc1],
            print_round(Rest, false, Top, Acc2)
    end;
print_round([], NewLine, _, Acc) ->
    add(not NewLine, io_lib:nl(), Acc).

add(false, _, Acc) -> Acc;
add(true, What, Acc) -> [What|Acc].


to_string({Y,M,D}) ->
    io_lib:format("~w-~2..0w-~2..0w",[Y,M,D]).

parse(String) ->
    Tokens = string:lexemes(String, "\n"),
    parse(Tokens, []).

parse([], Acc) ->
    lists:reverse(Acc);
parse(D, Acc) ->
    {Data, Rest} =
        try
            {_Line, D0} = get_integer(D),
            {Date, D1} = get_date(D0),
            {Club, D2} = get_string(D1),
            {_, D3} = get_type(D2),
            {Holes, D4} = get_integer(D3),
            {Points, D5} = get_integer(D4),
            {_Brutto, D6} = get_brutto(D5),
            {Hcp, D7} = get_float(D6),
            {_NewHcp, D8} = get_float(D7),
            Data0 = #hcp{date = Date, club=Club, holes=Holes, points=Points, hcp=Hcp},
            {Data0, D8}
        catch _:Err:ST ->
                io:format("~p: ~p~n",[Err,ST]),
                io:format("Data ~p~n",[lists:reverse(Acc)]),
                io:format("Error: ~P~n",[D,10]),
                exit(error)
        end,
    case Rest of
        [<<" ">>|Tail] -> parse(Tail, [Data|Acc]);
        [_|_] = Tail -> parse(Tail, [Data|Acc]);
        [] -> parse([], [Data|Acc])
    end.

get_integer([StringInt|Rest]) ->
    {Int, <<>>} = string:to_integer(StringInt),
    {Int, Rest}.

get_date([StringDate|Rest]) ->
    [YearS, MonthS, DayS] = string:lexemes(StringDate, "-"),
    {{binary_to_integer(YearS), binary_to_integer(MonthS), binary_to_integer(DayS)}, Rest}.

get_string([String|Rest]) ->
    {String, Rest}.

get_type([TypeOrHoles|Rest]=Tokens) ->
    case string:to_integer(TypeOrHoles) of
        {error, _} -> {TypeOrHoles, Rest};
        {_, <<>>} -> {<<>>, Tokens};
        {_, <<"-", _/binary>>} ->
            {<<>>, Rest}
    end.

get_brutto([BruttoOrHcp|Rest]=Tokens) ->
    case string:to_integer(BruttoOrHcp) of
        {Brutto, <<>>} -> {Brutto, Rest};
        {_, <<$,, _/binary>>} -> {"", Tokens}
    end.

get_float([StringFloat|Rest]) ->
    {Float, <<>>} = string:to_float(StringFloat),
    case StringFloat of
        <<"+", _/binary>> -> {-Float, Rest};
        _ -> {Float, Rest}
    end.

dan() ->
<<"1
2020-05-31
Bro-Bålsta Golfklubb
Sällskap
18
30
84
11,4
 
2
2020-05-22
Hofors Golfklubb
Sällskap
18
24
88
16,3
 
3
2020-05-20
GolfStar Golf Club
Sällskap
18
24
90
15,9
 
4
2019-09-06
Rättviks Golfklubb
18
35
7,6
 
5
2019-09-04
Rättviks Golfklubb
18
30
12,1
 
6
2019-09-02
Rättviks Golfklubb
18
29
13,0
 
7
2019-08-15
Cloud Golf Club
18
32
9,8
 
8
2019-07-31
Rättviks Golfklubb
18
20
20,4
 
9
2019-07-26
Bro-Bålsta Golfklubb
18
27
14,1
 
10
2019-07-19
Rättviks Golfklubb
18
36
5,8
 
11
2019-07-09
Bro-Bålsta Golfklubb
18
24
16,9
 
12
2019-07-06
Rättviks Golfklubb
18
36
5,8
 
13
2019-07-03
Rättviks Golfklubb
18
39
4,0
 
14
2019-07-02
Idrefjällens Golfklubb
18
30
11,1
 
15
2019-05-21
Cloud Golf Club
18
28
13,0
 
16
2018-10-06
Rättviks Golfklubb
18
42
2,2
 
17
2018-09-07
Rättviks Golfklubb
18
33
10,3
 
18
2018-09-05
Rättviks Golfklubb
18
38
5,8
 
19
2018-09-03
Rättviks Golfklubb
18
33
10,3
 
20
2018-07-25
Rättviks Golfklubb
18
39
5,8
">>.

