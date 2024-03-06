-module(gs_auth).
-export([init/0, login/1, verify_login/1]).

-export([make_passwd/1]).

-include("golf_stat.hrl").

-define(STORE, user_sessions).

init() ->
    ?STORE = ets:new(?STORE, [named_table, public]),
    ok.

store_session(#{headers := Headers}, UserName) ->
    <<"session_id=", Session/binary>> = maps:get(<<"cookie">>, Headers, undefined),
    true = ets:insert(?STORE, {Session, UserName}).

fetch_user(#{headers := Headers}) ->
    case maps:get(<<"cookie">>, Headers, undefined) of
        <<"session_id=", Session/binary>> ->
            case ets:lookup(?STORE, Session) of
                [{_, UserName}] -> UserName;
                _ -> undefined
            end;
        _ ->
            ?DBG("No cookie: ~p~n",[Headers]),
            undefined
    end.

make_passwd(Pwd) ->
    Sz = byte_size(Pwd),
    Pad = 64-Sz,
    Padded = <<Pwd/binary, 1:Pad/unit:8>>,
    crypto:hash(sha224, Padded).

login(#{params := Params} = Req) ->
    %% ?DBG(" ~p~n",[maps:get(body, Req)]),
    %% ?DBG(" ~P~n",[Params,20]),
    case Params of
        #{<<"username">> := Username, <<"password">> := Pwd} ->
            case golf_stat:is_user(Username, make_passwd(Pwd)) of
                ok ->
                    store_session(Req, Username),
                    {true, #{auth => true, username => Username}};
                {error, _} ->
                    false
            end;
        _ ->
            false
    end;

login(Req) ->
    case fetch_user(Req) of
        undefined ->
            %% ?DBG(" ~p~n",[maps:keys(Req)]),
            %% ?DBG(" ~p~n",[maps:get(body, Req)]),
            %% ?DBG(" ~p~n",[maps:get(params, Req, false)]),
            true;
        Username ->
            {true, #{auth => true, username => Username}}
    end.

verify_login(Req) ->
    case fetch_user(Req) of
        undefined ->
            %% ?DBG(" ~p~n",[maps:keys(Req)]),
            %% ?DBG(" ~p~n",[maps:get(body, Req)]),
            %% ?DBG(" ~p~n",[maps:get(params, Req, false)]),
            false;
        Username ->
            {true, #{auth => true, username => Username}}
    end.
