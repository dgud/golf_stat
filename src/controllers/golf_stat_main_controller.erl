-module(golf_stat_main_controller).

-export([
         index/1,
         login_view/1,
         get_courses/1,
         get_course/1,
         add_course/1,
         get_user_selections/1,
         get_stats_string/1,
         get_stats_diagram/1,
         add_user_round/1,
         add_user/1
        ]).

-include("golf_stat.hrl").

index(#{auth_data := #{auth := true, username := Username} = _Req}) ->
    %% ?DBG(" ~P~n",[_Req,20]),
    {ok, [{message, <<"Hello  ", Username/binary>>}]};
index(_Req) ->
    %% ?DBG("~n~p~n",[_Req]),
    {ok, [{message, "Please login!"}]}.

login_view(_Req) ->
    %% ?DBG(" ~P~n",[_Req,20]),
    {ok, [], #{view => login}}.


%% Json stuff

get_courses(_Req) ->
    reply({ok, golf_stat:courses()}).

get_course(#{bindings := #{<<"courseId">> := IdStr}}) ->
    Id = case string:to_integer(IdStr) of
             {Int, <<>>} -> Int;
             _ -> try gs_lib:base64_decode(IdStr) of
                      Course -> Course
                  catch _:_ ->
                          IdStr
                  end
         end,
    reply(golf_stat:course(Id)).

add_course(#{json := Req}) ->
    reply(golf_stat:add_course(json_akeys(Req)));
add_course(_) ->
    reply({error, <<"Invalid json">>}).

get_user_selections(#{auth_data := #{username := User}}) ->
    reply(golf_stat:user_round_selection(User));
get_user_selections(_Req) ->
    reply({error, <<>>}).

get_stats_string(#{auth_data := #{username := User}, json := #{<<"selection">> := Sel}}) ->
    reply(golf_stat:user_stats_string(User, Sel));
get_stats_string(_) ->
    reply({error, <<"Invalid selection">>}).


get_stats_diagram(#{auth_data := #{username := User}, json := #{<<"selection">> := Sel}}) ->
    case golf_stat:user_diagram_data(User, Sel) of
        {ok, {Labels, Stats}} ->
            reply({ok, #{labels => Labels, diagrams => Stats}});
        Error ->
            reply(Error)
    end;
get_stats_diagram(_) ->
    reply({error, <<"Invalid selection">>}).

add_user_round(#{auth_data := #{username := User}, json := Round}) ->
    reply(golf_stat:add_round(User, json_akeys(Round))).

add_user(#{json := #{<<"user">> := User, <<"pwd">> := Pwd}}) ->
    reply(golf_stat:new_user(User, gs_auth:make_passwd(Pwd))).

reply(ok) ->
    {json, <<>>};
reply({ok, Res}) ->
    {json, Res};
reply({error, Desc}) ->
    {status, 400, #{"content-type" => "text/plain"}, Desc}.


json_akeys(Req) when is_map(Req) ->
    #{binary_to_atom(Key) => json_akeys(Val) || Key := Val <- Req};
json_akeys(Req) when is_list(Req) ->
    [json_akeys(Val) || Val <- Req];
json_akeys(Val) ->
    Val.


