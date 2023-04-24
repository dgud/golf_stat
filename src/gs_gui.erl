-module(gs_gui).

-export([start/0, start/1, start_halt/1]).

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
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Golf Stats", [{size, {1800, 950}}]),
    wxFrame:connect(Frame, close_window),
    NB = wxNotebook:new(Frame, ?wxID_ANY),
    {AddText,Stat, StatSel, Diags} = stats_page(NB, Rounds),
    wxBookCtrlBase:addPage(NB, AddText, "View Statistics", []),
    AddCourse = gs_gui_round:start(NB, self(), Courses),
    wxBookCtrlBase:addPage(NB, AddCourse, "Add Round", []),
    {AddHcp,Hcp} = hcp_page(NB),
    wxBookCtrlBase:addPage(NB, AddHcp, "Calculate HCP", []),
    wxFrame:show(Frame),
    loop(#{file => File, frame => Frame, stat => Stat, stat_sel => StatSel, diag => Diags,
           rounds=>Rounds, courses=>Courses, hcp => Hcp}),
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
    {TextWin, Text, Choice} = text_stats(Main, Rounds),

    DSz = wxBoxSizer:new(?wxVERTICAL),
    DR1 = wxBoxSizer:new(?wxHORIZONTAL),
    DR2 = wxBoxSizer:new(?wxHORIZONTAL),
    D1 = diagram:start(Main),
    D2 = diagram:start(Main),
    D3 = diagram:start(Main),
    D4 = diagram:start(Main),
    [wxSizer:add(DR1, D, [{proportion, 1}, {flag, ?wxEXPAND}]) || D <- [D1,D2]],
    [wxSizer:add(DR2, D, [{proportion, 1}, {flag, ?wxEXPAND}]) || D <- [D3,D4]],
    [wxSizer:add(DSz, D, [{proportion, 1}, {flag, ?wxEXPAND}]) || D <- [DR1,DR2]],

    wxSizer:add(MainSz, TextWin, [{proportion, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, DSz, [{proportion, 3}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Main, MainSz),
    show_stats(undefined, "Total", Rounds, #{stat=>Text, diag=>[D1,D2,D3,D4]}),
    {Main, Text, Choice, [D1,D2,D3,D4]}.

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
    Y23 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2023 end, All),
    Y22 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2022 end, All),
    Y21 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2021 end, All),
    AllS = io_lib:format("All(~w)", [length(All)]),
    Y21Str = io_lib:format("2021(~w)", [length(Y21)]),
    Y22Str = io_lib:format("2022(~w)", [length(Y22)]),
    case All of
        [L1] ->
            {[All,Y21,Y22,[L1]], [AllS, Y21Str, Y22Str, date_str(L1,1)]};
        [L1,L2] ->
            {[All,Y21,Y22,[L2],[L1]], [AllS, Y21Str, Y22Str, date_str(L2,1), date_str(L1,1)]};
        [L1,L2|Rest] ->
            {Last, Lbls} = split(Rest, 5, 0, [[L2],[L1]], [date_str(L2,1), date_str(L1,1)]),
            {[All,Y21,Y22|Last], [AllS, Y21Str, Y22Str|Lbls]};
        [] ->
            {[All,Y21,Y22], [AllS, Y21Str, Y22Str]}
    end.

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

date_str([H|_], N) ->
    date_str(H,N);
date_str(#{date:=[_Y,M,D]}, N) ->
    io_lib:format("~w/~w(~w)", [D,M,N]).

update_diagram(Labels, AllData, Diags) ->
    [diagram:update(Diagram, Labels, Data) || {Diagram, Data} <- lists:zip(Diags, AllData)].

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
