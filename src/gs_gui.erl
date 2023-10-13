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

-spec start_halt() -> no_return().
start_halt() ->
    FileName = case os:getenv("USERNAME") of
                   false -> "player_stat.json";
                   Name -> Name ++ "_stat.json"
               end,
    LibDir = filename:absname(code:lib_dir(golf_stat)),
    Dir = filename:dirname(filename:dirname(LibDir)),
    File = filename:join(Dir, FileName),
    start(File, true).

-spec start_halt(string()) -> no_return().
start_halt(File) ->
    start(File, true).

-spec start(string(), boolean()) -> no_return().
start(File, Stop) ->
    Dir = filename:dirname(File),
    case golf_stat:start_link(#{dir => Dir}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    [User|_] = string:split(filename:basename(File), "_stat.json"),
    gui(User),
    quit(Stop),
    ok.

-spec quit(boolean()) -> no_return().
quit(true) ->  halt(0);
quit(false) -> ok.

gui(User) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Golf Stats", [{size, {1400, 950}}]),
    wxFrame:connect(Frame, close_window),
    NB = wxNotebook:new(Frame, ?wxID_ANY),
    try
        {ok, Menus} = golf_stat:user_round_selection(User),
        {AddText, Stat, StatSel, Diags} = stats_page(NB, Menus),
        show_stats(<<"All Rounds">>, User, #{stat=>Stat, diag=>Diags}),

        wxBookCtrlBase:addPage(NB, AddText, "View Statistics", []),
        AddCourse = gs_gui_round:start(NB, self()),
        wxBookCtrlBase:addPage(NB, AddCourse, "Add Round", []),
        {AddHcp,Hcp} = hcp_page(NB),
        wxBookCtrlBase:addPage(NB, AddHcp, "Calculate HCP", []),
        set_icon(Frame),
        wxFrame:show(Frame),
        loop(#{user => User, frame => Frame,
               stat => Stat, stat_sel => StatSel, diag => Diags,
               hcp  => Hcp})
    catch Err:Reason:ST ->
            io:format("~p ~P~n ~P~n",[Err,Reason,20,ST,15]),
            error(sorry)
    end,
    ok.

loop(#{frame := Frame, user:=User} = State0) ->
    receive
        {new_round, Round} ->
            {ok, IdStr} = golf_stat:add_round(User, Round),
            Choice = maps:get(stat_sel, State0),
            wxChoice:clear(Choice),
            {ok, Menus} = golf_stat:user_round_selection(User),
            wxChoice:insertStrings(Choice, Menus, 0),
            wxChoice:setStringSelection(Choice, IdStr),

            try show_stats(IdStr, User, State0)
            catch _:Err:ST -> io:format("Error ~P~n ~P~n",[Err, 20, ST, 20])
            end,

            %% NewRounds = [Round|Rounds],
            %% gs_stats:save_player(maps:get(file,State0), sort_rounds(NewRounds)),
            %% Name = round_id(Round),
            %% Choice = maps:get(stat_sel, State0),

            %% Def = length(default_menus(Rounds)),
            %% wxChoice:insert(Choice, Name, Def), %% After the default stuff
            %% wxChoice:setSelection(Choice, Def),
            %% State = State0#{rounds:=NewRounds},
            %% show_stats(Def, Name, NewRounds, State),
            loop(State0);
        {new_course, Course} ->
            {ok, _} = golf_stat:add_course(Course),
            loop(State0#{});
        #wx{event=#wxCommand{type=command_choice_selected, cmdString=Str}} ->
            try show_stats(unicode:characters_to_binary(Str), User, State0)
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

stats_page(NB, Menues) ->
    Main = wxPanel:new(NB),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Background = wxScrolledWindow:new(Main),
    wxScrolledWindow:setScrollRate(Background, 5, 25),
    wxScrolledWindow:setBackgroundColour(Background, {250, 250, 250}),

    {TextWin, Text, Choice} = text_stats(Main, Menues),

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
    {Main, Text, Choice, Diags}.

text_stats(Main, MenuRounds) ->
    Win = wxPanel:new(Main),
    LSz = wxBoxSizer:new(?wxVERTICAL),
    Choice = wxChoice:new(Win, ?COURSE, [{size, {400,-1}}, {choices, MenuRounds}]),
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

show_stats(Selection, User, #{stat:=Stat, diag:=Ds}) ->
    {ok, {LBs, DD}} = golf_stat:user_diagram_data(User, Selection),
    [diagram:update(Diagram, LBs, Data) || {Diagram, Data} <- lists:zip(Ds, DD)],
    {ok, StatString} = golf_stat:user_stats_string(User, Selection),
    wxTextCtrl:setValue(Stat, StatString).

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
