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
    Win = wxWindow:new(Parent, ?wxID_ANY, [{style,Style}, {size, {-1, 400}}]),
    wxWindow:setBackgroundStyle(Win, ?wxBG_STYLE_PAINT),
    wxWindow:setBackgroundColour(Win, {250, 250, 250}),
    wxWindow:connect(Win, paint, [callback]),
    Font = make_font(),
    Brushes = make_brushes(),
    Pens = make_pens(),
    {Win, #{parent=>Parent, type=>bar, win=>Win, labels=>[], data=>[],
            font=>Font, brushes=>{Pens,Brushes}}}.

handle_sync_event(#wx{event = #wxPaint{}}, _, #{win:=Win}=State) ->
    DC = wxPaintDC:new(Win),
    wxDC:clear(DC),
    try draw(DC, State)
    catch _E:R:ST ->
            io:format("~p:~p ~p ~P~n",[?MODULE,?LINE, R, ST, 20])
    end,
    wxPaintDC:destroy(DC),
    ok.

handle_event(_Ev, State) ->
    {noreply, State}.

handle_call({update, Labels, Data}, _, #{win:=Win}=State) ->
    wxWindow:refresh(Win),
    {reply, ok, State#{labels:=Labels, data:=Data}}.

-spec draw(term(), map()) -> no_return().  %% Dialyzer error without info

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw(DC, #{win:=Win, font:=[_Font0,Font1]=Fonts, brushes:=Brushes, labels:=Labels, data:=Data} = State) ->
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

    case maps:get(type, State, graph) of
        bar -> drawbars(X0, XM, Y0, YM, Max, Canvas, Fonts, Brushes, Labels, Data);
        graph ->
            try drawgraphs(X0, XM, Y0, YM, Max, Canvas, Fonts, Brushes, Labels, Data)
            catch _:E:ST -> io:format("~w ~0p~n",[E,ST])
            end
    end,

    wxGraphicsContext:destroy(Canvas),
    ok.

drawgraphs(X0,XM,Y0,YM, Max, Canvas, [Font0,Font1], {Pens,_}, Labels, Data) ->
    Step = round((XM-X0-10) / (length(Labels)-1)),

    Scale = fun(Y) -> ((Max-Y)/Max)*(YM-Y0)+Y0 end,
    Xs = lists:seq(round(X0), round(XM), round(Step)),

    DrawGraph = fun({_Label, D}, PenIndex) ->
                        wxGraphicsContext:setPen(Canvas, element(PenIndex, Pens)),
                        Ys = [Scale(Y) || Y <- D],
                        %% io:format("~p ~p ~p~n", [Xs,length(Xs),length(Ys)]),
                        wxGraphicsContext:drawLines(Canvas, lists:zip(Xs,Ys)),
                        PenIndex + 1
                end,
    lists:foldl(DrawGraph, 1, Data),
    wxGraphicsContext:setFont(Canvas, Font1, {0, 0, 50}),
    [ wxGraphicsContext:drawText(Canvas, Label, X-20, YM+5) || {Label, X} <- lists:zip(Labels, Xs)],

    DrawLabel = fun({Label,_Data}, {X, ColIndex}) ->
                           wxGraphicsContext:setFont(Canvas, Font0, element(ColIndex, colors())),
                           {StrW, StrH, _, _} = wxGraphicsContext:getTextExtent(Canvas, Label),
                           wxGraphicsContext:drawText(Canvas, Label, X, YM+StrH+5),
                           {X+StrW+10, ColIndex+1}
                end,
    lists:foldl(DrawLabel, {X0, 1}, Data),
    ok.


drawbars(X0,XM,Y0,YM, Max, Canvas, [Font0,Font1], {_, Brushes}, Labels, Data) ->
    BW0 = ((XM-X0)/max(1,(2*length(Labels))))/max(length(Data),1),
    BW = max(6, min(10, BW0)),
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
                        max(Start+StrW, X)+BW+2
                end,

    lists:foldl(DrawBoxes, X0+5, Data),
    XL = fun(Label, {N, X, Y}) ->
                 wxGraphicsContext:setFont(Canvas, Font0, element(N, colors())),
                 {StrW, _, _, _} = wxGraphicsContext:getTextExtent(Canvas, Label),
                 wxGraphicsContext:drawText(Canvas, Label, X, Y),
                 {N+1, X+StrW+20, Y}
         end,
    lists:foldl(XL, {1, X0, YM+25}, Labels).



%% transpose([{Key, List}|Rest]) ->
%%     transpose(Rest, [[E] || E <- List], [Key]).

%% transpose([{Key, List}|Rest], Acc0, Keys) ->
%%     Acc = transpose2(List, Acc0),
%%     transpose(Rest, Acc, [Key|Keys]);
%% transpose([], Acc, Keys) ->
%%     {[lists:reverse(L) || L <- Acc],
%%      lists:reverse(Keys)}.

%% transpose2([H|T], [L1|Rest]) ->
%%     [[H|L1]|transpose2(T,Rest)];
%% transpose2([], []) ->
%%     [].


make_font() ->
    DefFont = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    DefSize = wxFont:getPointSize(DefFont),
    DefFamily = wxFont:getFamily(DefFont),
    [wxFont:new(1 * (DefSize+1), DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
     wxFont:new(1 * (DefSize-1), DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)].

make_brushes() ->
    list_to_tuple([wxBrush:new(C) || C <- tuple_to_list(colors())]).

make_pens() ->
    list_to_tuple([wxPen:new(C, [{width, 2}]) || C <- tuple_to_list(colors())]).

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
