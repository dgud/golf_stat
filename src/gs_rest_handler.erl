%%% @author Dan Gudmundsson
%%% @copyright (C) 2023, Dan Gudmundsson
%%% @doc
%%%        Rest handler
%%% @end
%%% Created : 30 Aug 2023 by Dan
%%%-------------------------------------------------------------------


%% https://ninenines.eu/docs/en/cowboy/2.10/guide/rest_handlers/
%% https://github.com/dronowar/erlang_rest_api/blob/master/README.md

-module(gs_rest_handler).
-behavior(cowboy_rest).

-include("golf_stat.hrl").

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Callback Callbacks
-export([from_json/2]).
-export([from_text/2]).

-import(gs_lib, [json_encode/1, json_decode/1, base64_decode/1, reply/3]).

%% Helpes
%% -import(helper, [get_body/2, get_model/3, reply/3, pwd2hash/1]).

%% Cowboy REST callbacks
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/plain">>, from_text},
      {<<"text/html">>, from_text},
      {{<<"application">>, <<"json">>, []}, from_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, from_json}
     ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

from_text(Req, [courses] = State) ->
    Courses = golf_stat:courses(),
    {json_encode(Courses), Req, State};

from_text(Req, [course] = State) ->
    IdStr = cowboy_req:binding(courseId, Req),
    try
        Id = case string:to_integer(IdStr) of
                 {Int, <<>>} -> Int;
                 _ -> base64_decode(IdStr)
             end,
        case golf_stat:course(Id) of
            {ok, Course} ->
                {json_encode(Course), Req, State};
            {error, ErrString} ->
                get_error(ErrString, Req, State)
        end
    catch _:_Reason:_ST ->
            %% ?DBG("IdStr: ~p~n",[IdStr]),
            %% ?DBG("~p~n  ~p~n",[_Reason, _ST]),
            Error = <<"Bad format course id, support number or base64 encoded utf8 string!"/utf8>>,
            get_error(Error, Req, State)
    end;

from_text(Req, [hello]=State) ->
    %% ?DBG("~p ~p~n",[Req, State]),
    Msg = <<"Hello Text Caller">>,
    {json_encode(Msg), Req, State};

from_text(Req, State) ->
    ?DBG("~p ~p~n",[Req, State]),
    Msg = <<"Hello Text Caller">>,
    {json_encode(Msg), Req, State}.

from_json(Req0, [add_course] = State) ->
    case get_json_body(Req0) of
        {ok, Json, Req1} ->
            case golf_stat:add_course(Json) of
                {ok, Cs} ->
                    post_reply(Cs, Req1, State);
                {error, Desc} ->
                    post_error(Desc, Req1, State)
            end;
        {error, Desc, Req1} ->
            post_error(Desc, Req1, State)
    end;
from_json(Req, State) ->
    ?DBG("~p ~n", [Req]),
    Msg = <<"Hello Json Caller">>,
    {json_encode(Msg), Req, State}.

post_reply(Msg, Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req0),
    Req = cowboy_req:set_resp_body(json_encode(Msg), Req1),
    {true, Req, State}.

post_error(Error, Req0, State) when is_binary(Error) ->
    %% Req1 = cowboy_req:set_resp_header(#{<<"content-type">> => <<"application/json">>}, Req0),
    Req = cowboy_req:set_resp_body(Error, Req0),
    {false, Req, State}.

get_error(Error, Req, State) when is_binary(Error) ->
    %% Header = #{<<"content-type">> => <<"application/json">>}
    {stop, cowboy_req:reply(400, #{}, Error, Req), State}.

get_json_body(Req0) ->
    case cowboy_req:read_urlencoded_body(Req0) of
        {ok, [{Body, true}], Req} ->
            try
                Data = json_decode(Body),
                {ok, Data, Req}
            catch _:_ ->
                    {error, <<"Invalid json"/utf8>>, Req}
            end;
        {ok, [], Req} ->
            {error, <<"Missing body"/utf8>>, Req};
        {ok, _, Req} ->
            {error, <<"Bad request"/utf8>>, Req}
    end.

%% register_from_json(Req, State) ->
%%     {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

%%     %% Check request body
%%     case get_body(Body, Req1) of
%%         {ok, Input, _Req} ->
%%             %% Validate body json and fields
%%             Model = [
%%                      {<<"email">>, required, string, email, [non_empty,
%%                                                              fun(V) ->
%%                                                                      validator:email(V)
%%                                                              end
%%                                                             ]},
%%                      {<<"pass">>, required, string, pass, [non_empty, 
%%                                                            fun(V) -> 
%%                                                                    validator:min_length(6, V)
%%                                                            end
%%                                                           ]},
%%                      {<<"fname">>, required, string, fname, [non_empty]},
%%                      {<<"lname">>, required, string, lname, [non_empty]}                
%%                     ],
%%             Emodel = get_model(Input, Model, Req1),

%%             %% Check model result
%%             case Emodel of
%%                 {error, Reason} ->
%%                     Req3 = reply(412, {Reason}, Req1),
%%                     {false, Req3, State};
%%                 {error, empty, Req4} ->
%%                     {false, Req4, State};
%%                 {ok, _} ->
%%                     %% Perform Registration
%%                     case registration(Emodel, Req1) of
%%                         {ok, User, Req5} ->
%%                             {true, reply(200, User, Req5), State};
%%                         {error, Req6} ->
%%                             {false, Req6, State}
%%                     end
%%             end;
%%         {error, empty, Req2} ->
%%             {false, Req2, State}

%%     end.

%% register_from_text(Req, State) ->
%%     #{token := Token} = cowboy_req:match_qs([{token, nonempty, undefined}], Req),
%%     case Token of
%%         undefined ->
%%             {[], reply(400, <<"Token mismatch">>, Req), State};
%%         _Token ->
%%             case cowboy_session:get(<<"register">>, Req) of
%%                 {undefined, Req1} ->
%%                     {[], reply(400, <<"Token expired">>, Req1), State};
%%                 {Register, Req1} ->
%%                     SToken = maps:get(token, Register),
%%                     erlang:display([Token, SToken]),
%%                     case SToken =:= Token of
%%                         true ->
%%                             {ok, Req2} = cowboy_session:set(register, undefined, Req1),
%%                             case persist:check_user(pgdb, maps:get(email, Register)) of
%%                                 false ->
%%                                     Email = maps:get(email, Register),
%%                                     Pass = maps:get(pass, Register),
%%                                     Fname = maps:get(fname, Register),
%%                                     Lname = maps:get(lname, Register),
%%                                     case persist:add_user(pgdb, Email, Fname, Lname, Pass) of
%%                                         {ok, 1} ->
%%                                             User = #{email => Email, fname => Fname, lname => Lname},
%%                                             {ok, Req3} = cowboy_session:set(<<"user">>, User, Req2),
%%                                             {jiffy:encode(User), Req3, State};
%%                                         _ ->
%%                                             {[], reply(500, <<"Cannot add new user in database">>, Req2)} 
%%                                     end;
%%                                 _ ->
%%                                     {[], reply(400, <<"User already exists">>, Req2)} 
%%                             end;
%%                         false -> 
%%                             {[], reply(400, <<"Wrong token!">>, Req1), State}
%%                     end
%%             end
%%     end.

%% %% Registration functions
%% registration(Emodel, Req) ->
%%     %% Auth middleware
%%     case middleware:allready_auth(Req) of
%%         {false, Req1} ->
%%             {ok, Data} = Emodel,
%%             case persist:check_user(pgdb, maps:get(email, Data)) of
%%                 false ->
%%                     Pass = maps:get(pass, Data),
%%                     Register_safe_pass = maps:update(pass, pwd2hash(Pass), Data),
%%                     Register = maps:put(token, random(64), Register_safe_pass),
%%                     {ok, Req2} = cowboy_session:set(<<"register">>, Register, Req1),
%%                     {ok, Register, Req2};
%%                 _ ->
%%                     {error, reply(400, <<"User already exists">>, Req1)}
%%             end;
%%         {true, _User, Req3} -> {error, Req3}
%%     end.

%% random(Len) ->
%%     Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
%%     ChrsSize = size(Chrs),
%%     F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
%%     list_to_binary(lists:foldl(F, "", lists:seq(1, Len))).
