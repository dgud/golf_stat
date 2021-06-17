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
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Golf Stats", [{size, {900, 950}}]),
    wxFrame:connect(Frame, close_window),
    NB = wxNotebook:new(Frame, ?wxID_ANY),
    {AddText,Stat, StatSel} = stats_page(NB, Rounds),
    wxBookCtrlBase:addPage(NB, AddText, "View Statistics", []),
    AddCourse = gs_gui_round:start(NB, self(), Courses),
    wxBookCtrlBase:addPage(NB, AddCourse, "Add Round", []),
    {AddHcp,Hcp} = hcp_page(NB),
    wxBookCtrlBase:addPage(NB, AddHcp, "Calculate HCP", []),
    wxFrame:show(Frame),
    loop(#{file => File, frame => Frame, stat => Stat, stat_sel => StatSel,
           rounds=>Rounds, courses=>Courses, hcp => Hcp}),
    ok.

loop(#{frame := Frame, rounds:=Rounds, stat:=Stat} = State0) ->
    receive
        {new_round, Round} ->
            NewRounds = [Round|Rounds],
            gs_stats:save(maps:get(file,State0), NewRounds),
            wxTextCtrl:setValue(Stat, gs_stats:print_stats([Round])),
            Name = round_id(Round),
            Choice = maps:get(stat_sel, State0),
            wxChoice:insert(Choice, Name, 1), %% Total is always 0
            wxChoice:setSelection(Choice, 1),
            loop(State0#{rounds:=NewRounds});
        {new_course, Course} ->
            NewCourses = [Course|maps:get(courses, State0)],
            RFile = maps:get(file, State0),
            Dir = filename:dirname(RFile),
            gs_stats:save_courses(NewCourses, Dir),
            loop(State0#{courses:=NewCourses});
        #wx{event=#wxCommand{type=command_choice_selected, commandInt=0}} ->
            wxTextCtrl:setValue(Stat, gs_stats:print_stats(Rounds)),
            loop(State0);
        #wx{event=#wxCommand{type=command_choice_selected, commandInt=Rnd}} ->
            Round = lists:nth(Rnd,Rounds),
            wxTextCtrl:setValue(Stat, gs_stats:print_stats([Round])),
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
    RoundNames = ["Total"| [ round_id(Course) || Course <- Rounds]],
    Choice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, RoundNames}]),
    wxChoice:connect(Choice,command_choice_selected),
    wxChoice:setSelection(Choice, 0),
    wxSizer:add(Sz, Choice, [{border, 10}, {flag, ?wxALL}]),
    Font = wxFont:new(12, ?wxMODERN, ?wxNORMAL, ?wxNORMAL),
    Text = wxTextCtrl:new(Win, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY}]),
    wxWindow:setFont(Text, Font),
    wxSizer:add(Sz, Text, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    case gs_stats:print_stats(Rounds) of
        ignore -> ok;
        StatsStr -> wxTextCtrl:setValue(Text, StatsStr)
    end,
    wxWindow:setSizerAndFit(Win, Sz),
    {Win, Text, Choice}.

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
