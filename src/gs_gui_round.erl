-module(gs_gui_round).

-export([start/3, add_new_course_dialog/1]).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("wx/include/wx.hrl").

-define(NEXT, 1).
-define(PREVIOUS, 2).
-define(RESET, 3).
-define(DONE, 4).
-define(COURSE, 5).

start(Notebook, Parent, Courses) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Courses], []).


init([NoteBook, Parent, Courses]) ->
    Win = wxPanel:new(NoteBook),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(Sz, 5),
    CourseNames = ["Add New Course"|[Name || #{name:=Name} <- Courses]],
    CourseChoice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, CourseNames}]),
    wxChoice:connect(CourseChoice,command_choice_selected),
    wxSizer:add(Sz, CourseChoice, [{border, 50}, {flag, ?wxLEFT}]),
    wxSizer:addSpacer(Sz, 20),
    Stat = wxStaticText:new(Win, ?wxID_ANY, current(1, 0, 0, 0)),
    wxSizer:add(Sz, Stat, [{border, 50}, {flag, ?wxLEFT}]),
    lists:foldl(fun(Strs, Acc) -> add_row(Strs, Win, Sz, Acc), Acc+10 end, 10, gs_stats:shots()),
    cont_buttons(Win, Sz),

    wxWindow:connect(Win, command_button_clicked),
    wxWindow:setSizerAndFit(Win, Sz),
    {Win, #{stat=>Stat, parent=>Parent, courses=>Courses, course=>undefined, pars=>[],
            hole_cnt=>1, hole=>gs_stats:empty(), round=>#{}, total=>0, frame => NoteBook}}.

handle_event(#wx{id=?DONE, event=#wxCommand{type=command_button_clicked}}, #{stat:=Stat} = State0) ->
    State = handle_click(?DONE, State0),
    wxStaticText:setLabel(Stat, current(State)),
    {noreply, State};

handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}},
             #{stat:=Stat, course:=CName, frame:=Frame} = State0) ->
    case CName =:= undefined of
        true ->
            MB = wxMessageDialog:new(Frame, "Select course first"),
            wxMessageDialog:showModal(MB),
            {noreply, State0};
        _ ->
            State = handle_click(Id, State0),
            update_shots(State),
            wxStaticText:setLabel(Stat, current(State)),
            {noreply, State}
    end;

handle_event(#wx{obj=CC, event=#wxCommand{type=command_choice_selected, commandInt=0}}, State0) ->
    State = handle_add_new_course(CC, State0),
    {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_choice_selected, commandInt=Sel}},
             #{stat:=Stat} = State0) ->
    #{name:=Name, pars:=Pars} = lists:nth(Sel, maps:get(courses, State0)),
    State = State0#{course:=Name, pars:=Pars},
    wxStaticText:setLabel(Stat, current(State)),
    {noreply, State};

handle_event(Msg, State0) ->
    io:format("Unhandled Msg: ~p~n",[Msg]),
    {noreply, State0}.

handle_call(Msg, _, State0) ->
    io:format("Unhandled call: ~p~n",[Msg]),
    {noreply, State0}.

handle_cast(Msg, State0) ->
    io:format("Unhandled cast: ~p~n",[Msg]),
    {noreply, State0}.

handle_info(Msg, State0) ->
    io:format("Unhandled info: ~p~n",[Msg]),
    {noreply, State0}.


add_hole(#{hole_cnt:=HoleNo, hole:=Hole, round:=Round, pars:=Pars} = State) ->
    Par = lists:nth(HoleNo, Pars),
    State#{round:=gs_stats:add_hole(HoleNo, gs_stats:set_hole_data(par, Par, Hole), Round)}.

handle_click(?NEXT, #{hole_cnt:=HoleNo, pars:=Pars} = State0) ->
    Next = HoleNo+1,
    case Next > length(Pars) of
        true -> State0;
        false ->
            #{round:=Round} = State = add_hole(State0),
            Hole = gs_stats:get_hole(Next, Round),
            State#{hole_cnt := Next, hole:=Hole}
    end;
handle_click(?PREVIOUS, #{hole_cnt:=HoleNo, round:=Round} = State0) ->
    case HoleNo > 1 of
        true ->
            State = add_hole(State0),
            Prev = HoleNo-1,
            Hole = gs_stats:get_hole(Prev, Round),
            State#{hole_cnt:=Prev, hole:=Hole};
        false ->
            State0
    end;
handle_click(?RESET, #{hole := Hole, total := Total} = State0) ->
    Shots = gs_stats:no_shots(Hole),
    State0#{hole := gs_stats:empty(), total := Total-Shots};
handle_click(?DONE, #{total:=Total, hole_cnt:=Cnt, course:=Course, parent:=Parent} = State) ->
    case Course /= undefined andalso Cnt > 1 andalso Total > 0 of
        false -> State;
        true ->
            #{round:=Round0} = add_hole(State),
            Round1 = gs_stats:set_round_data(course, unicode:characters_to_list(Course), Round0),
            Round = gs_stats:set_round_data(date, tuple_to_list(date()), Round1),
            Parent ! {new_round, Round},
            State#{hole_cnt=>1, hole=>gs_stats:empty(), round=>#{}, total=>0}
    end;
handle_click(Button, #{hole:=Hole, total:=Total} = State) ->
    ShotId = Button div 10,
    Shot = gs_stats:key(ShotId),
    Result = case Button rem 10 of
                 1 -> bad;
                 2 -> good;
                 3 -> perfect
             end,
    Prev = maps:get(Shot, Hole),
    Update = Hole#{Shot => maps:update_with(Result, fun(V) -> V + 1 end, Prev)},
    State#{hole:=Update, total:=Total+1}.

current(#{hole_cnt:=No, hole:=Hole, total:=Total, pars:=Pars}) ->
    Shots = gs_stats:no_shots(Hole),
    Par = try lists:nth(No, Pars) catch _:_ -> 0 end,
    current(No, Par, Shots, Total).

current(Hole, Par, Shots, Total) ->
    io_lib:format("Hole: ~.2w  Par: ~.2w   Shots: ~.2w  Total: ~.2w", [Hole, Par, Shots, Total]).

update_shots(#{hole:=Hole}) ->
    Shots = lists:seq(1, length(gs_stats:shots())),
    Update = fun(ShotId) ->
                     Added = maps:get(gs_stats:key(ShotId), Hole, #{default => 0}),
                     Count = lists:sum(maps:values(Added)),
                     StatTxt = wx:typeCast(wxWindow:findWindowById(ShotId*10), wxStaticText),
                     wxStaticText:setLabel(StatTxt, integer_to_list(Count))
             end,
    [Update(Id) || Id <- Shots].


%% Gui stuff
add_row({Txt1, Txt2}, Panel, Sz, Id) ->
    RowSz = wxBoxSizer:new(?wxHORIZONTAL),
    Text = wxStaticText:new(Panel, ?wxID_ANY, Txt1),
    Border = [{border, 5}, {flag, ?wxALL}],
    Align = [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALL}, {border, 5}],
    wxSizer:add(RowSz, Text, Align),
    wxSizer:setItemMinSize(RowSz, Text, 80, -1),
    Count = wxStaticText:new(Panel, Id, "0"),
    wxSizer:add(RowSz, Count, Align),
    case Txt1 of
        "Drop" ->
            wxSizer:add(RowSz, wxButton:new(Panel, Id+1, [{label, "Drop"}]), Border);
        _ ->
            wxSizer:add(RowSz, wxButton:new(Panel, Id+1, [{label, "Bad"}]), Border),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+2, [{label, "Ok"}]), Border),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+3, [{label, "Great"}]), Border)
    end,
    wxSizer:add(RowSz, wxStaticText:new(Panel, ?wxID_ANY, Txt2), Align),
    wxSizer:add(Sz, RowSz, [{border, 10}, {flag, ?wxLEFT}]).

cont_buttons(Panel, Sz) ->
    RowSz = wxBoxSizer:new(?wxHORIZONTAL),
    Border = [{border, 5}, {flag, ?wxALL}],

    wxSizer:addSpacer(RowSz, 80),
    wxSizer:add(RowSz, wxButton:new(Panel, ?PREVIOUS, [{label, "Previous Hole"}]), Border),
    wxSizer:add(RowSz, wxButton:new(Panel, ?NEXT, [{label, "Next Hole"}]), Border),
    wxSizer:addSpacer(RowSz, 100),
    wxSizer:add(RowSz, wxButton:new(Panel, ?RESET, [{label, "Reset Hole"}]), Border),
    wxSizer:add(RowSz, wxButton:new(Panel, ?DONE, [{label, "Done"}]), Border),

    wxSizer:add(Sz, RowSz, [{border, 10}, {flag, ?wxLEFT}]).

handle_add_new_course(Choice, #{stat:=Stat, frame:=Frame, parent:=Parent} = State0) ->
    case add_new_course_dialog(Frame) of
        ignore ->
            State0;
        #{name:=Name, pars:=Pars} = Course ->
            wxChoice:insert(Choice, Name, 0),
            wxChoice:setSelection(Choice, 0),
            Parent ! {new_course, Course},
            State = State0#{course:=Name, pars:=Pars},
            wxStaticText:setLabel(Stat, current(State)),
            State
    end.

add_new_course_dialog(Frame) ->
    Win = wxDialog:new(Frame, -1, "Add Course"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    NameRow = wxBoxSizer:new(?wxHORIZONTAL),
    NameLabel = wxStaticText:new(Win,?wxID_ANY, "Name"),
    Name = wxTextCtrl:new(Win, 500, [{size, {200, -1}}]),
    wxSizer:add(NameRow, NameLabel),
    wxSizer:setItemMinSize(NameRow, NameLabel, 80, -1),
    wxSizer:add(NameRow, Name),
    wxSizer:add(Sizer, NameRow),
    wxSizer:add(Sizer, wxStaticText:new(Win,?wxID_ANY, "Add par for each hole")),
    AddHole = fun(No) ->
                      Row = wxBoxSizer:new(?wxHORIZONTAL),
                      RowText = wxStaticText:new(Win,?wxID_ANY, integer_to_list(No) ++ ":"),
                      LB = wxChoice:new(Win, 500+No, [{choices, ["3","4","5"]}]),
                      wxSizer:add(Row, RowText),
                      wxSizer:add(Row, LB, [{proportion, 0}]),
                      wxSizer:setItemMinSize(Row, RowText, 50, -1),
                      wxSizer:add(Sizer, Row, [{border, 10}, {flag, ?wxLEFT}]),
                      LB
              end,
    Input = [{Hole, AddHole(Hole)} || Hole <- lists:seq(1,18)],
    Buttons = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Buttons, wxButton:new(Win, ?wxID_OK), [{border, 10}, {flag, ?wxALL}]),
    wxSizer:add(Buttons, wxButton:new(Win, ?wxID_CANCEL), [{border, 10}, {flag, ?wxALL}]),
    wxSizer:add(Sizer, Buttons),
    wxWindow:setSizerAndFit(Win, Sizer),
    case wxDialog:showModal(Win) of
        ?wxID_OK ->
            Course = wxTextCtrl:getValue(Name),
            Pars0 = [{Hole,wxControlWithItems:getSelection(LB)+3} || {Hole,LB} <- Input],
            try check(Pars0) of
                Pars ->
                    #{name => unicode:characters_to_binary(Course), pars=>Pars}
            catch throw:ignore ->
                    ignore
            end;
        ?wxID_CANCEL ->
            ignore
    end.

check([{_,Par}|Rest]) when Par > 2 ->
    [Par|check(Rest)];
check([]) ->
    [];
check([{10,_}|_]) -> [];
check([{13,_}|_]) -> [];
check(Bad) ->
    io:format("Ignore course with bad input: ~p ~n",[Bad]),
    throw(ignore).
