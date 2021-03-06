%%% @author Dan <dangud@gmail.com>
%%% @copyright (C) 2022, Dan
%%% @doc
%%%       Draw diagrams
%%% @end
%%% Created : 30 Jun 2022 by Dan <dgud@Dan>

-module(diagram).
-export([start/1, update/3]).


%% wx_object callbacks
-export([init/1, %% terminate/2,  code_change/3,
	 %%handle_info/2,
         handle_call/3,
         %%handle_cast/2,
	 handle_event/2,
         handle_sync_event/3]).

-export([test_me/0]).

-include_lib("wx/include/wx.hrl").

start(Parent) ->
    wx_object:start_link(?MODULE, [Parent], []).

update(Diag, Labels, Data) ->
    wx_object:call(Diag, {update, Labels, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Parent]) ->
    wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
    Style = ?wxFULL_REPAINT_ON_RESIZE bor ?wxCLIP_CHILDREN,
    Win = wxWindow:new(Parent, ?wxID_ANY, [{style,Style}]),
    wxWindow:setBackgroundStyle(Win, ?wxBG_STYLE_PAINT),
    wxWindow:connect(Win, paint, [callback]),
    Font = make_font(),
    Brushes = make_brushes(),
    {Win, #{parent=>Parent, win=>Win, labels=>[], data=>[], font=>Font, brushes=>Brushes}}.

handle_sync_event(#wx{event = #wxPaint{}}, _, #{win:=Win}=State) ->
    DC = wxPaintDC:new(Win),
    wxDC:clear(DC),
    draw(DC, State),
    wxPaintDC:destroy(DC),
    ok.

handle_event(_Ev, State) ->
    {noreply, State}.

handle_call({update, Labels, Data}, _, #{win:=Win}=State) ->
    wxWindow:refresh(Win),
    {reply, ok, State#{labels:=Labels, data:=Data}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw(DC, #{win:=Win, font:=[Font0,Font1], brushes:=Brushes, labels:=Labels, data:=Data}) ->
    {W,H} = wxWindow:getClientSize(Win),
    Canvas = wxGraphicsContext:create(DC),
    wxGraphicsContext:setPen(Canvas, ?wxBLACK_PEN),
    wxGraphicsContext:setFont(Canvas, Font1, {0, 0, 50}),

    {TW, TH, _, _} = wxGraphicsContext:getTextExtent(Canvas, "100"),

    X0 = TW + 30.0,
    Y0 = 10.0,
    YM = H - 50,
    XM = W - 10,

    wxGraphicsContext:drawLines(Canvas, [{X0,Y0-5}, {X0, YM}, {XM, YM}]),

    Max0 = lists:foldl(fun({_, Vs}, Max) -> max(Max, lists:max(Vs)) end, 0.0, Data),
    Marks = if Max0 > 100 -> lists:seq(0, 200, 50);
               Max0 > 50  -> lists:seq(0, 100, 25);
               Max0 > 20  -> lists:seq(0, 50, 10);
               Max0 > 15  -> lists:seq(0, 20, 5);
               Max0 > 10  -> lists:seq(0, 15, 5);
               Max0 > 1   -> lists:seq(0, 10, 2);
               true -> [0,1]
            end,
    Max = lists:last(Marks),

    YL = fun(V) ->
                 Str = integer_to_list(V),
                 {StrW, _, _, _} = wxGraphicsContext:getTextExtent(Canvas, Str),
                 Where = ((Max-V)/Max)*(YM-Y0)+Y0,
                 wxGraphicsContext:drawText(Canvas, Str, X0-15-StrW, Where - TH/2.0),
                 wxGraphicsContext:drawLines(Canvas, [{X0,Where}, {XM, Where}]),
                 case V > 0 of
                     true ->
                         Half = V-hd(tl(Marks))/2,
                         Where2 = ((Max-Half)/Max)*(YM-Y0)+Y0,
                         wxGraphicsContext:drawLines(Canvas, [{X0,Where2}, {XM, Where2}]);
                     false ->
                         ignore
                 end
         end,
    lists:foreach(YL, Marks),

    XL = fun(Label, {N, X, Y}) ->
                 wxGraphicsContext:setFont(Canvas, Font0, element(N, colors())),
                 {StrW, _, _, _} = wxGraphicsContext:getTextExtent(Canvas, Label),
                 wxGraphicsContext:drawText(Canvas, Label, X, Y),
                 {N+1, X+StrW+TW/3, Y}
         end,
    lists:foldl(XL, {1, X0, YM+TH+5}, Labels),

    BW0 = ((XM-X0)/length(Data)-5)/length(Labels)-3,
    BW = max(6, BW0),
    DrawBox = fun(V, {N, X}) ->
                      Y = ((Max-V)/Max)*(YM-Y0)+Y0,
                      wxGraphicsContext:setBrush(Canvas, element(N, Brushes)),
                      wxGraphicsContext:drawLines(Canvas, [{X,YM+1}, {X+BW, YM+1},
                                                           {X+BW, Y}, {X,Y}, {X,YM+1}]),
                      {N+1, X+BW+3}
              end,

    DrawBoxes = fun({Label, D}, Start) ->
                        wxGraphicsContext:setFont(Canvas, Font1, {0, 0, 50}),
                        {StrW, _, _, _} = wxGraphicsContext:getTextExtent(Canvas, Label),
                        wxGraphicsContext:drawText(Canvas, Label, Start+5, YM+5),

                        {_, X} = lists:foldl(DrawBox, {1, Start}, D),
                        max(Start+StrW, X)+5
                end,

    lists:foldl(DrawBoxes, X0+5, Data),

    ok.

make_font() ->
    DefFont = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    DefSize = wxFont:getPointSize(DefFont),
    DefFamily = wxFont:getFamily(DefFont),
    [wxFont:new(1 * (DefSize+1), DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
     wxFont:new(1 * (DefSize-1), DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)].

make_brushes() ->
    list_to_tuple([wxBrush:new(C) || C <- tuple_to_list(colors())]).

colors() ->
    {{240, 100, 100}, {0, 128, 0},     {25, 45, 170},  {200, 130, 0},
     {180, 180, 70},  {100, 240, 240},{240, 100, 240}, {160, 40,  40},
     {100, 100, 240}, {140, 140, 0},  {25, 200, 100},  {120, 25, 240},
     {255, 140, 163}, {25, 120, 120}, {120, 25, 120},  {110, 90, 60}
    }.

test_me() ->
    Frame = wxFrame:new(wx:new(), -1, "Test diagram", [{size, {800, 600}}]),
    Diag = start(Frame),
    update(Diag,  ["Fool", "Barzal", "Baz ASDl", "ok"],
           [{"d1", [50,20,80]}, {"data2", [60,70,80]}, {"d3",[40, 50, 20]}]),
    wxFrame:show(Frame),
    ok.
