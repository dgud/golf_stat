-module(gs_gui_round).

-export([start/2, add_new_course_dialog/1]).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("wx/include/wx.hrl").

-define(NEXT, 1).
-define(PREVIOUS, 2).
-define(RESET, 3).
-define(RESET_SHOT, 4).
-define(COURSE, 5).
-define(DATE, 6).
-define(DONE, 7).

start(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).


input_desc() ->
    "Input shots in order from tee to hole.\n\n"
        "Categorizing shots is personal but I use the following: \n"
        " - Bad: Shots that causes a drop or a bogey on the hole\n"
        " - Good: Normal good shots\n"
        " - Great: Shots that saves a result or are just perfect\n".

init([NoteBook, Parent]) ->
    Win = wxPanel:new(NoteBook),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(Sz, 5),

    InfoCtrl = wxStaticText:new(Win, ?wxID_ANY, input_desc()),
    wxSizer:add(Sz, InfoCtrl, [{border, 20}, {flag, ?wxALL}]),

    CourseNames = ["Add New Course"|golf_stat:courses()],
    CourseChoice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, CourseNames}]),
    wxChoice:connect(CourseChoice,command_choice_selected),
    ChSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(ChSz, wxStaticText:new(Win, ?wxID_ANY, "Course: ", [{size, {80,-1}}])),
    wxSizer:add(ChSz, CourseChoice),
    wxSizer:add(Sz, ChSz, [{border, 15}, {flag, ?wxLEFT}]),

    DP = wxDatePickerCtrl:new(Win, ?DATE, [{style, ?wxDP_DROPDOWN}, {size, {100, -1}}]),
    wxDatePickerCtrl:connect(DP, date_changed, []),
    wxSizer:add(Sz, DP, [{border, 95}, {flag, ?wxLEFT}]),

    wxSizer:addSpacer(Sz, 30),
    Stat = wxStaticText:new(Win, ?wxID_ANY, current(1, 0, 0, 0)),
    wxSizer:add(Sz, Stat, [{border, 50}, {flag, ?wxLEFT}]),
    lists:foldl(fun(Strs, Acc) -> add_row(Strs, Win, Sz, Acc), Acc+10 end, 10, gs_stats:shots()),
    cont_buttons(Win, Sz),
    ShotOrder = wxStaticText:new(Win, ?wxID_ANY, "Hit shots", [{size, {100, -1}}]),
    wxStaticText:setFont(ShotOrder, gs_gui:make_mono_font(13)),
    wxSizer:add(Sz, ShotOrder, [{border, 50}, {flag, ?wxLEFT}]),

    wxWindow:connect(Win, command_button_clicked),
    wxWindow:setSizerAndFit(Win, Sz),
    {Win, #{stat => Stat, parent => Parent, frame => NoteBook, shot_order => ShotOrder,
            course => undefined, date => tuple_to_list(date()),
            pars => [], hole_cnt => 1,
            round => #{holes => []}, total => 0, shots => []}}.

handle_event(#wx{id=?DONE, event=#wxCommand{type=command_button_clicked}}, State0) ->
    State = handle_click(?DONE, State0),
    write_info(State),
    {noreply, State};

handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}},
             #{course:=CName, frame:=Frame} = State0) ->
    case CName =:= undefined of
        true ->
            MB = wxMessageDialog:new(Frame, "Select course first"),
            wxMessageDialog:showModal(MB),
            {noreply, State0};
        _ ->
            State = handle_click(Id, State0),
            update_shots(State),
            write_info(State),
            {noreply, State}
    end;

handle_event(#wx{obj=CC, event=#wxCommand{type=command_choice_selected, commandInt=0}}, State0) ->
    State = handle_add_new_course(CC, State0),
    {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_choice_selected, commandInt=Sel}}, State0) ->
    {ok, #{name:=Name, pars:=Pars}} = golf_stat:course(Sel-1),
    State = State0#{course:=Name, pars:=Pars},
    write_info(State),
    {noreply, State};

handle_event(#wx{id=?DATE, event=#wxDate{date={Date,_Time}}}, State) ->
    {noreply, State#{date := tuple_to_list(Date)}};

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


add_hole(#{shots:=[]} = State) ->
    State;
add_hole(#{hole_cnt:=HoleNo, round:=Round, pars:=Pars, shots:=Shots} = State) ->
    Par = lists:nth(HoleNo, Pars),
    Hole0 = gs_stats:set_hole_data(no, HoleNo, #{}),
    Hole1 = gs_stats:set_hole_data(par, Par, Hole0),
    Hole  = gs_stats:set_hole_data(shots, lists:reverse(Shots), Hole1),
    State#{round:=gs_stats:add_hole(HoleNo, Hole, Round)}.

handle_click(?NEXT, #{hole_cnt:=HoleNo, pars:=Pars} = State0) ->
    Next = HoleNo+1,
    case Next > length(Pars) of
        true -> State0;
        false ->
            #{round:=Round} = State = add_hole(State0),
            #{no := Next, shots := Shots} = gs_stats:get_hole(Next, Round),
            State#{hole_cnt := Next, shots := lists:reverse(Shots)}
    end;
handle_click(?PREVIOUS, #{hole_cnt:=HoleNo, round:=Round} = State0) ->
    case HoleNo > 1 of
        true ->
            State = add_hole(State0),
            Prev = HoleNo-1,
            #{no := Prev, shots := Shots} = gs_stats:get_hole(Prev, Round),
            State#{hole_cnt := Prev, shots := lists:reverse(Shots)};
        false ->
            State0
    end;
handle_click(?RESET, #{shots := Shots, total := Total} = State) ->
    N = length(Shots),
    State#{total := Total-N, shots := []};
handle_click(?DONE, #{total:=Total, hole_cnt:=Cnt, course:=Course, date:=Date, parent:=Parent} = State) ->
    case Course /= undefined andalso Cnt > 1 andalso Total > 0 of
        false -> State;
        true ->
            #{round:=Round0} = add_hole(State),
            Round1 = gs_stats:set_round_data(course, unicode:characters_to_list(Course), Round0),
            Round = gs_stats:set_round_data(date, Date, Round1),
            Parent ! {new_round, Round},
            State#{hole_cnt=>1, hole=>gs_stats:empty(), shots => [],
                   round => #{holes => []}, total=>0}
    end;
handle_click(Button, #{shots:=Shots, total:=Total} = State)
  when (Button rem 10) =:= ?RESET_SHOT ->
    ShotId = Button div 10,
    Club = gs_stats:key(ShotId),
    case lists:splitwith(fun(#{club := Last}) -> Last =:= Club end, Shots) of
        {[], _} -> % Can't reset club, reset hole
            N = length(Shots),
            State#{total := Total-N, shots := []};
        {Del, Keep} ->
            N = length(Del),
            State#{total := Total-N, shots := Keep}
    end;
handle_click(Button, #{shots:=Shots, total:=Total} = State) ->
    ShotId = Button div 10,
    Club = gs_stats:key(ShotId),
    Result = case Button rem 10 of
                 1 -> bad;
                 2 -> good;
                 3 -> perfect
             end,
    State#{shots:=[#{club => Club, Result => 1}|Shots], total:=Total+1}.


write_info(#{stat:=StatCtrl, shot_order:=ShotCtrl, shots:=Shots} = State) ->
    wxStaticText:setLabel(StatCtrl, current(State)),
    wxStaticText:setLabel(ShotCtrl, shot_order_string(Shots)).

current(#{hole_cnt:=No, shots:=Shots, total:=Total, pars:=Pars}) ->
    ShotCnt = length(Shots),
    Par = try lists:nth(No, Pars) catch _:_ -> 0 end,
    current(No, Par, ShotCnt, Total).

current(Hole, Par, Shots, Total) ->
    io_lib:format("Hole: ~.2w  Par: ~.2w   Shots: ~.2w  Total: ~.2w", [Hole, Par, Shots, Total]).

update_shots(#{shots:=Shots}) ->
    ShotTypes = lists:seq(1, length(gs_stats:shots())),
    Update = fun(ShotId) ->
                     Count = length(lists:filter(fun(#{club := Club}) -> Club =:= gs_stats:key(ShotId) end, Shots)),
                     StatTxt = wx:typeCast(wxWindow:findWindowById(ShotId*10), wxStaticText),
                     wxStaticText:setLabel(StatTxt, integer_to_list(Count))
             end,
    [Update(Id) || Id <- ShotTypes].

shot_order_string(Shots) ->
    [io_lib:format("~.2w: ~12s  ~s~n", [No, Club, verdict(Shot)]) ||
        {No, #{club := Club} = Shot} <- lists:enumerate(lists:reverse(Shots))].

verdict(#{bad := _}) -> bad;
verdict(#{good := _}) -> good;
verdict(#{perfect := _}) -> perfect.


%% Gui stuff
add_row({Txt1, Txt2}, Panel, Sz, Id) ->
    RowSz = wxBoxSizer:new(?wxHORIZONTAL),
    Text = wxStaticText:new(Panel, ?wxID_ANY, Txt1),
    Border = [{border, 3}, {flag, ?wxALL}],
    Align = [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALL}, {border, 5}],
    wxSizer:add(RowSz, Text, Align),
    wxSizer:setItemMinSize(RowSz, Text, 80, -1),
    Count = wxStaticText:new(Panel, Id, "0"),
    wxSizer:add(RowSz, Count, Align),
    case Txt1 of
        "Drop" ->
            wxSizer:add(RowSz, wxButton:new(Panel, Id+1, [{label, "Drop"}]), Border),
            wxSizer:addSpacer(RowSz, 5),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+4, [{label, "Reset"}]), Border);
        _ ->
            wxSizer:add(RowSz, wxButton:new(Panel, Id+1, [{label, "Bad"}]), Border),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+2, [{label, "Ok"}]), Border),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+3, [{label, "Great"}]), Border),
            wxSizer:addSpacer(RowSz, 5),
            wxSizer:add(RowSz, wxButton:new(Panel, Id+4, [{label, "Reset"}]), Border)
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
    wxSizer:add(RowSz, wxButton:new(Panel, ?DONE, [{label, "Save Round"}]), Border),

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
