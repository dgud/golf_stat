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

loop(#{frame := Frame, rounds:=Rounds, stat:=Stat} = State0) ->
    receive
        {new_round, Round} ->
            NewRounds = [Round|Rounds],
            gs_stats:save(maps:get(file,State0), NewRounds),
            wxTextCtrl:setValue(Stat, gs_stats:print_stats("", [Round])),
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
    Win = wxPanel:new(NB),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    RoundNames = default_menus() ++ [ round_id(Course) || Course <- Rounds],
    Choice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, RoundNames}]),
    wxChoice:connect(Choice,command_choice_selected),
    wxChoice:setSelection(Choice, 0),
    wxSizer:add(Sz, Choice, [{border, 10}, {flag, ?wxALL}]),
    Font = wxFont:new(12, ?wxMODERN, ?wxNORMAL, ?wxNORMAL),
    Text = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY}]),
    wxWindow:setFont(Text, Font),
    wxSizer:add(Sz, Text, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    DSz = wxBoxSizer:new(?wxVERTICAL),
    D1 = diagram:start(Win),
    wxSizer:add(DSz, D1, [{proportion, 1}, {flag, ?wxEXPAND}]),
    D2 = diagram:start(Win),
    wxSizer:add(DSz, D2, [{proportion, 1}, {flag, ?wxEXPAND}]),
    HSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(HSz, Sz, [{proportion, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(HSz, DSz, [{proportion, 3}, {flag, ?wxEXPAND}]),
    wxWindow:setSizerAndFit(Win, HSz),
    show_stats(undefined, "Total", Rounds, #{stat=>Text, diag=>[D1,D2]}),
    {Win, Text, Choice, [D1,D2]}.

default_menus() ->
    ["Total", "2022", "2021", "Last 5", "Last 10"].

show_stats(Sel, String, All, #{stat:=Stat, diag:=Ds}) ->
    [Y21,Y22,L5,L10] = split_rounds(All),
    DL = ["All","2021","2022","Last 5", round_id(hd(All))],

    {Desc, Rs, DiDa, DLs} =
        case String of
            "Total" ->
                DD = diagram_data([All,Y21,Y22,L5], hd(All)),
                {"All Rounds", All, DD, DL};
            "2022" = Str ->
                DD = diagram_data([All,Y21,Y22,L5], hd(All)),
                {"Year " ++ Str, Y22, DD, DL};
            "2021" = Str ->
                DD = diagram_data([All,Y21,Y22,L5], hd(All)),
                {"Year " ++ Str, Y21, DD, DL};
            "Last 5" = Str ->
                DD = diagram_data([All,Y21,Y22,L5], hd(All)),
                {Str, L5, DD, DL};
            "Last 10" = Str ->
                DD = diagram_data([All,Y21,Y22,L10], hd(All)),
                DL2 = ["All","2021","2022","Last 10", round_id(hd(All))],
                {Str, L10, DD, DL2};
            _ ->
                Def = length(default_menus()),
                %% io:format("~p ~p => ~p~n", [Sel, Def, Sel-Def+1]),
                Round = lists:nth(Sel-Def+1,All),
                DD = diagram_data([All,Y21,Y22,L5], Round),
                DL2 = ["All","2021","2022","Last 5", round_id(Round)],
                {"", [Round], DD, DL2}
        end,
    update_diagram(DLs,DiDa,Ds),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats(Desc, Rs)).

split_rounds(All) ->
    Y22 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2022 end, All),
    Y21 = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2021 end, All),
    try
        {L5, _} = lists:split(5, All),
        {L10, _} = lists:split(10, All),
        [Y21,Y22,L5,L10]
    catch _:_ ->
            [Y21,Y22,[],[]]
    end.

update_diagram(Labels, {D11, D21}, [D1,D2]) ->
    diagram:update(D1, Labels, D11),
    diagram:update(D2, Labels, D21).

diagram_data([All,Y21,Y22,L5], Round) ->
    Rounds = [Y21,Y22,L5,[Round]],
    {D1,D2} = gs_stats:diagram_data(All),
    {D11, D21} = merge_data(Rounds, [{T,[D]} || {T,D} <- D1], [{T,[D]} || {T,D} <- D2]),
    {D11, D21}.

merge_data([R|Rs], D1, D2) ->
    {R1,R2} = gs_stats:diagram_data(R),
    merge_data(Rs, merge_data2(R1, D1), merge_data2(R2, D2));
merge_data([], D1, D2) ->
    {[{T,lists:reverse(D)} || {T,D} <- D1],
     [{T,lists:reverse(D)} || {T,D} <- D2]}.

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
