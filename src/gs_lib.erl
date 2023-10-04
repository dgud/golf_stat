%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson
%%% @copyright (C) 2023, Dan Gudmundsson
%%% @doc
%%%        Server holding data and answering calls
%%% @end
%%% Created : 30 Aug 2023 by Dan 
%%%-------------------------------------------------------------------
-module(gs_lib).

-export(
   [json_encode/1,
    json_decode/1,
    base64_encode/1,
    base64_decode/1,
    reply/3,
    enumerate/1, enumerate/2, enumerate/3
   ]).


json_encode(ListOrMap) ->
    jsone:encode(ListOrMap, [native_utf8, {space,0}]).

json_decode(Bin) ->
    jsone:decode(Bin, [{keys, atom}]).

base64_encode(Bin) when is_binary(Bin) ->
    %base64:encode(Bin, #{mode => urlsafe}).
    base64:encode(Bin).

base64_decode(Bin) when is_binary(Bin) ->
    %base64:decode(Bin, #{mode => urlsafe}).
    base64:decode(Bin).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, json_encode(Body), Req).

-spec enumerate(List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      T :: term().
enumerate(List1) ->
    enumerate(1, 1, List1).

-spec enumerate(Index, List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      T :: term().
enumerate(Index, List1) ->
    enumerate(Index, 1, List1).

-spec enumerate(Index, Step, List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      Step :: integer(),
      T :: term().
enumerate(Index, Step, List1) when is_integer(Index), is_integer(Step) ->
    enumerate_1(Index, Step, List1).

enumerate_1(Index, Step, [H|T]) ->
    [{Index, H}|enumerate_1(Index + Step, Step, T)];
enumerate_1(_Index, _Step, []) ->
    [].

