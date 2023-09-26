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
    reply/3
   ]).


json_encode(ListOrMap) ->
    jsone:encode(ListOrMap, [native_utf8, {space,0}]).

json_decode(Bin) ->
    jsone:decode(Bin, [{keys, atom}]).

base64_encode(Bin) when is_binary(Bin) ->
    base64:encode(Bin, #{mode => urlsafe}).

base64_decode(Bin) when is_binary(Bin) ->
    base64:decode(Bin, #{mode => urlsafe}).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, json_encode(Body), Req).

