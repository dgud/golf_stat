-module(gs_gui).

-export([start/0, start/1, start_halt/0, start_halt/1]).

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
    Old = gs_stats:read(File),
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
        wxFrame:show(Frame),
        loop(#{file => File, frame => Frame, stat => Stat, stat_sel => StatSel, diag => Diags,
               rounds=>Rounds, courses=>Courses, hcp => Hcp})
    catch Err:Reason:ST ->
            io:format("~p ~P~n ~P~n",[Err,Reason,20,ST,30]),
            error(sorry)
    end,
    ok.

loop(#{frame := Frame, rounds:=Rounds} = State0) ->
    receive
        {new_round, Round} ->
            NewRounds = [Round|Rounds],
            gs_stats:save(maps:get(file,State0), NewRounds),
            show_stats(undefined, "Total", NewRounds, State0),
            Name = round_id(Round),
            Choice = maps:get(stat_sel, State0),

            Def = length(default_menus()),
            wxChoice:insert(Choice, Name, Def), %% After the default stuff
            wxChoice:setSelection(Choice, Def),
            loop(State0#{rounds:=NewRounds});
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
            io:format("~p: Closing~n", [?MODULE]),
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
                Str <- ["Long game", "Short game", "Putting", "Number of putts", "Saves and Drops", "Hole Stats", "Shots"]],

    Fix = fun(ST) ->
                  Font = wxStaticText:getFont(ST),
                  wxFont:setPointSize(Font,15),
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
    show_stats(undefined, "Total", Rounds, #{stat=>Text, diag=>Diags}),
    {Main, Text, Choice, Diags}.

text_stats(Main, Rounds) ->
    Win = wxPanel:new(Main),
    LSz = wxBoxSizer:new(?wxVERTICAL),
    RoundNames = default_menus() ++ [ round_id(Course) || Course <- Rounds],
    Choice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, RoundNames}]),
    wxChoice:connect(Choice,command_choice_selected),
    wxChoice:setSelection(Choice, 0),
    wxSizer:add(LSz, Choice, [{border, 10}, {flag, ?wxALL}]),
    Font = wxFont:new(12, ?wxMODERN, ?wxNORMAL, ?wxNORMAL),
    Text = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY}]),
    wxWindow:setFont(Text, Font),
    wxSizer:add(LSz, Text, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),    
    wxWindow:setSizer(Win, LSz),
    {Win, Text, Choice}.

default_menus() ->
    ["Total", "2023", "2022", "2021", "Last 5", "Last 10"].

show_stats(Sel, String, All, #{stat:=Stat, diag:=Ds}) ->
    {DiDa,LBs} = split_rounds(All),
    DD = diagram_data(DiDa),

    {Desc, Rs} =
        case String of
            "Total" ->
                {io_lib:format("All rounds (~w)", [length(All)]), All};
            "2023" = Str ->
                Y22 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2023 end, All),
                {io_lib:format("Year ~s (~w)", [Str, length(Y22)]), Y22};
            "2022" = Str ->
                Y22 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2022 end, All),
                {io_lib:format("Year ~s (~w)", [Str, length(Y22)]), Y22};
            "2021" = Str ->
                Y21 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2021 end, All),
                {io_lib:format("Year ~s (~w)", [Str, length(Y21)]), Y21};
            "Last 5" = Str ->
                {L5, _} = lists:split(5, All),
                {Str, L5};
            "Last 10" = Str ->
                {L10, _} = lists:split(10, All),
                {Str, L10};
            _ ->
                Def = length(default_menus()),
                %% io:format("~p ~p => ~p~n", [Sel, Def, Sel-Def+1]),
                Round = lists:nth(Sel-Def+1,All),
                {"", [Round]}
        end,
    update_diagram(LBs,DD,Ds),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats(Desc, Rs)).

split_rounds(All) ->
    {CurrentYear, _, _} = erlang:date(),
    CombinedYearData = split_years(CurrentYear, All),
    case CombinedYearData of
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

split_years(Current, Rounds) ->
    {YearRs,Older} = lists:splitwith(fun(#{date := [Year|_]}) -> Year =:= Current end, Rounds),
    Data = {YearRs, date_str(Current, length(YearRs))},
    case Older of
        [] ->
            [Data];
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

hcp_page(NB) ->
    Win = wxPanel:new(NB),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    Result = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY}]),
    wxSizer:add(Sz, Result, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    Desc = "Copy your rounds from mingolf.se from \"Min Handicap\" button to text area below",
    wxSizer:add(Sz, wxStaticText:new(Win,?wxID_ANY, Desc)),
    Input = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2}]),
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
