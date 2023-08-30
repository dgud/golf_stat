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
    reply/3
   ]).


json_encode(ListOrMap) ->
    jsone:encode(ListOrMap, [native_utf8, {space,0}]).

json_decode(Bin) ->
    jsone:decode(Bin, [{keys, atom}]).


reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, json_encode(Body), Req).

