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
            try show_stats(Rnd, Str, Rounds, Stat)
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
    case gs_stats:print_stats("Summary of all rounds", Rounds) of
        ignore -> ok;
        StatsStr -> wxTextCtrl:setValue(Text, StatsStr)
    end,
    wxWindow:setSizerAndFit(Win, Sz),
    {Win, Text, Choice}.

default_menus() ->
    ["Total", "2022", "2021", "Last 5", "Last 10"].

show_stats(_, "Total", Rounds, Stat) ->
    wxTextCtrl:setValue(Stat, gs_stats:print_stats("All Rounds", Rounds));
show_stats(_, "2022" = Str, Rounds0, Stat) ->
    Rounds = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2022 end, Rounds0),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats("Year " ++ Str, Rounds));
show_stats(_, "2021" = Str, Rounds0, Stat) ->
    Rounds = lists:filter(fun(#{date := [Year|_]}) -> Year =:= 2021 end, Rounds0),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats("Year " ++ Str, Rounds));
show_stats(_, "Last 5" = Str, Rounds0, Stat) ->
    {Rounds, _} = lists:split(5, Rounds0),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats(Str, Rounds));
show_stats(_, "Last 10" = Str, Rounds0, Stat) ->
    {Rounds, _} = lists:split(10, Rounds0),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats(Str, Rounds));
show_stats(Sel, _, Rounds, Stat) ->
    Def = length(default_menus()),
    %% io:format("~p ~p => ~p~n", [Sel, Def, Sel-Def+1]),
    Round = lists:nth(Sel-Def+1,Rounds),
    wxTextCtrl:setValue(Stat, gs_stats:print_stats("", [Round])).


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
