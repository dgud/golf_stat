-module(gs_auth).
-export([init/0, login/1, check_login/1, verify_login/1]).

-export([make_passwd/1]).

-include("golf_stat.hrl").

-define(STORE, user_sessions).

init() ->
    ?STORE = ets:new(?STORE, [named_table, public]),
    ok.

store_session(#{headers := Headers}, UserName) ->
    <<"session_id=", Session/binary>> = maps:get(<<"cookie">>, Headers, undefined),
    ?DBG("Store: ~s id: ~s~n", [UserName, Session]),
    true = ets:insert(?STORE, {Session, UserName}).

fetch_user(#{headers := Headers}) ->
    case maps:get(<<"cookie">>, Headers, undefined) of
        <<"session_id=", Session/binary>> ->
            case ets:lookup(?STORE, Session) of
                [{_, UserName}] ->
                    ?DBG("User: ~s id: ~s~n", [UserName, Session]),
                    UserName;
                _ ->
                    ?DBG("Fail: ~s~n", [Session]),
                    [io:format("  ~p~n", [Entry]) || Entry <- ets:tab2list(?STORE)],
                    undefined
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
            ?DBG("Login: ~s pwd *******~n", [Username]),
            case golf_stat:is_user(Username, make_passwd(Pwd)) of
                ok ->
                    store_session(Req, Username),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, <<"Login error">>}
    end.

check_login(Req) ->
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
