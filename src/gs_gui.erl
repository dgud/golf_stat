-module(gs_gui).

-export([start/0, start/1, start_halt/0, start_halt/1]).
-export([make_mono_font/1]).

-include_lib("wx/include/wx.hrl").
-define(COURSE, 5).

start() ->
    usage().

usage() ->
    io:format("Error: need a filename~n").

start("") ->
    usage();
start(File) ->
    start(File, false).

start_halt() ->
    FileName = case os:getenv("USERNAME") of
               false -> "player.json";
               Name -> Name ++ ".json"
           end,
    LibDir = filename:absname(code:lib_dir(golf_stat)),
    Dir = filename:dirname(filename:dirname(LibDir)),
    File = filename:join(Dir, FileName),
    start(File, true).

start_halt(File) ->
    start(File, true).

start(File, Stop) ->
    Old = gs_stats:read_player(File),
    Dir = filename:dirname(File),
    gui(File, gs_stats:read_courses(Dir), Old),
    Stop andalso halt(0),
    ok.

gui(File, Courses, Rounds) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Golf Stats", [{size, {1400, 950}}]),
    wxFrame:connect(Frame, close_window),
    NB = wxNotebook:new(Frame, ?wxID_ANY),
    try
        {AddText,Stat, StatSel, Diags} = stats_page(NB, Rounds),
        wxBookCtrlBase:addPage(NB, AddText, "View Statistics", []),
        AddCourse = gs_gui_round:start(NB, self(), Courses),
        wxBookCtrlBase:addPage(NB, AddCourse, "Add Round", []),
        {AddHcp,Hcp} = hcp_page(NB),
        wxBookCtrlBase:addPage(NB, AddHcp, "Calculate HCP", []),
        set_icon(Frame),
        wxFrame:show(Frame),
        loop(#{file => File, frame => Frame, stat => Stat, stat_sel => StatSel, diag => Diags,
               rounds=>Rounds, courses=>Courses, hcp => Hcp})
    catch Err:Reason:ST ->
            io:format("~p ~P~n ~P~n",[Err,Reason,20,ST,15]),
            error(sorry)
    end,
    ok.

loop(#{frame := Frame, rounds:=Rounds} = State0) ->
    receive
        {new_round, Round} ->
            NewRounds = [Round|Rounds],
            gs_stats:save_player(maps:get(file,State0), sort_rounds(NewRounds)),
            Name = round_id(Round),
            Choice = maps:get(stat_sel, State0),

            Def = length(default_menus(Rounds)),
            wxChoice:insert(Choice, Name, Def), %% After the default stuff
            wxChoice:setSelection(Choice, Def),
            State = State0#{rounds:=NewRounds},
            show_stats(Def, Name, NewRounds, State),
            loop(State);
        {new_course, Course} ->
            NewCourses = [Course|maps:get(courses, State0)],
            RFile = maps:get(file, State0),
            Dir = filename:dirname(RFile),
            gs_stats:save_courses(NewCourses, Dir),
            loop(State0#{courses:=NewCourses});
        #wx{event=#wxCommand{type=command_choice_selected, commandInt=Rnd, cmdString=Str}} ->
            try show_stats(Rnd, Str, Rounds, State0)
            catch _:Err:ST -> io:format("Error ~P~n ~P~n",[Err, 20, ST, 20])
            end,
            loop(State0);
        #wx{event=#wxCommand{type=command_text_updated, cmdString=Text}} ->
            display_hcp(maps:get(hcp, State0), Text),
            loop(State0);
        #wx{event=#wxClose{}} ->
            wxWindow:'Destroy'(Frame),
            %% io:format("~p: Closing~n", [?MODULE]),
            ok;
        Msg ->
            io:format("~p: Msg ~p~n", [?MODULE, Msg]),
            loop(State0)
    end.

stats_page(NB, Rounds) ->
    Main = wxPanel:new(NB),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Background = wxScrolledWindow:new(Main),
    wxScrolledWindow:setScrollRate(Background, 5, 25),
    wxScrolledWindow:setBackgroundColour(Background, {250, 250, 250}),

    {TextWin, Text, Choice} = text_stats(Main, Rounds),

    DSz = wxBoxSizer:new(?wxVERTICAL),
    Diags = [diagram:start(Background) ||
                _ <- lists:seq(1,7)],

    Texts = [wxStaticText:new(Background, ?wxID_ANY, Str) ||
                Str <- ["Statistics", "Scores",
                        "Long game", "Short game",
                        "Putting", "Number of putts",
                        "Hole Stats"]],

    Fix = fun(ST) ->
                  Font = wxStaticText:getFont(ST),
                  wxFont:setPointSize(Font,14),
                  wxStaticText:setFont(ST, Font),
                  wxStaticText:setBackgroundColour(Background, {250, 250, 250})
          end,
    [Fix(St) || St <- Texts],

    Widgets = lists:flatten(lists:zipwith(fun(X, Y) -> [X,Y] end, Texts, Diags)),
    [wxSizer:add(DSz, D, [{flag, ?wxEXPAND}]) || D <- Widgets],

    wxWindow:setSizer(Background, DSz),
    wxSizer:layout(DSz),

    wxSizer:add(MainSz, TextWin, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, Background, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Main, MainSz),
    show_stats(undefined, "All Rounds", Rounds, #{stat=>Text, diag=>Diags}),
    {Main, Text, Choice, Diags}.

text_stats(Main, Rounds) ->
    Win = wxPanel:new(Main),
    LSz = wxBoxSizer:new(?wxVERTICAL),
    RoundNames = default_menus(Rounds) ++ [ round_id(Course) || Course <- Rounds],
    Choice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, RoundNames}]),
    wxChoice:connect(Choice,command_choice_selected),
    wxChoice:setSelection(Choice, 0),
    wxSizer:add(LSz, Choice, [{border, 10}, {flag, ?wxALL}]),
    Font = make_mono_font(12),
    %% io:format("~p: FontName: ~p~n",[?LINE, wxFont:getNativeFontInfoUserDesc(Font)]),
    Text = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY}]),
    wxWindow:setFont(Text, Font),
    wxSizer:add(LSz, Text, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),    
    wxWindow:setSizer(Win, LSz),
    {Win, Text, Choice}.

default_menus(Rounds) ->
    Found = [Year || #{date := [Year|_]} <- Rounds],
    {ThisYear,_,_} = date(),
    Years = lists:reverse(lists:usort([ThisYear|Found])),
    YearsStrings = [integer_to_list(Year) || Year <- Years],
    LastRounds = ["Last " ++ integer_to_list(Rs) || Rs <- [2,3,5,10,20], Rs =< length(Rounds)],
    ["All Rounds"] ++ YearsStrings ++ LastRounds.

show_stats(Sel, String, All, #{stat:=Stat, diag:=Ds}) ->
    Year = case string:to_integer(String) of
               {error, _} -> undefined;
               {Int, _} when is_integer(Int), 1970 < Int -> Int;
               _ -> undefined
           end,

    Def = length(default_menus(All)),

    {Desc, Rounds, Other} =
        case {String, is_integer(Year)} of
            {"All Rounds", false} ->
                {io_lib:format("All rounds (~w)", [length(All)]), All, []};
            {"Last " ++ Num, false} ->
                {N, _} = string:to_integer(Num),
                {L5, Rest} = lists:split(N, All),
                {String, L5, Rest};
            {_, false} when Def =< Sel ->
                %% io:format("~p ~p => ~p~n", [Sel, Def, Sel-Def+1]),
                #{course:=Course0} = Round = lists:nth(Sel-Def+1,All),
                Rest = lists:delete(Round, All),
                [Course1|_] = string:split(Course0, "Golf"),
                [Course|_] = string:split(Course1, "golf"),
                {string:trim(Course), [Round], Rest};
            {_, true} ->
                {YearData, Rest} = lists:partition(fun(#{date := [Y|_]}) -> Y =:= Year end, All),
                {io_lib:format("Year ~s (~w)", [String, length(YearData)]), YearData, Rest}
        end,

    {DiDa,LBs} = split_rounds(Desc, Rounds, Other, Other =/= [] andalso length(Rounds) =/= 1),
    DD = diagram_data(DiDa),

    update_diagram(LBs,DD,Ds),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats(Desc, Rounds, Other)).

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

update_diagram(Labels, AllData, Diags) ->
    [diagram:update(Diagram, Labels, Data) || {Diagram, Data} <- lists:zip(Diags, AllData)].

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

round_id(#{course:=Name, date:=[Y,M,D]}) ->
    io_lib:format("~ts ~w-~2..0w-~2..0w", [Name,Y,M,D]).

sort_rounds(Rounds) ->
    Order = fun(#{date := A},#{date := B}) -> A >= B end,
    lists:sort(Order, Rounds).

set_icon(Frame) ->
    Filename = "golf-hole.png",
    Dir = code:priv_dir(golf_stat),
    File = filename:join(Dir, Filename),
    Image = wxImage:new(File),
    Bitmap = wxBitmap:new(Image),
    Icon = wxIcon:new(),
    wxIcon:copyFromBitmap(Icon, Bitmap),
    wxImage:destroy(Image),
    wxBitmap:destroy(Bitmap),
    wxFrame:setIcon(Frame, Icon).


hcp_page(NB) ->
    Win = wxPanel:new(NB),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    Desc = "Copy your (top 20) rounds from mingolf.se below \"Min Handicap\" to the green area at the bottom",
    Result = wxTextCtrl:new(Win, ?wxID_ANY,
                            [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY},
                             {value, Desc}
                            ]),
    wxSizer:add(Sz, Result, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    Input = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2}]),
    wxWindow:setBackgroundColour(Input, {144,238,144}),
    wxTextCtrl:connect(Input, command_text_updated),
    wxSizer:add(Sz, Input, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    wxWindow:setSizerAndFit(Win, Sz),
    {Win, Result}.

display_hcp(Widget, Result) ->
    try hcp:calc(unicode:characters_to_binary(Result)) of
        Str when is_list(Str) ->
            wxTextCtrl:setValue(Widget, Str)
    catch Exit:Reason:ST ->
            io:format("~p:~P~n  ~P~n",[Exit,Reason,40,ST,40]),
            wxTextCtrl:setValue(Widget, "Couldn't parse data")
    end.

make_mono_font(Size) ->
    case os:type() of
        {unix, linux} ->
            wxFont:new(Size, ?wxDEFAULT, ?wxNORMAL, ?wxNORMAL, [{face, "Noto Sans Mono CJK KR"}]);
        _ ->
            wxFont:new(Size, ?wxMODERN, ?wxNORMAL, ?wxNORMAL)
    end.
