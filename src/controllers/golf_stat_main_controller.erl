-module(golf_stat_main_controller).

-export([
         index/1,
         login_view/1,
         courses_view/1,
         course_view/1,
         add_course_view/1,
         add_user_round_view/1,
         user_stats_string_view/1,
         user_stats_diagram_view/1,

         login/1,

         %% Json api
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
    {ok, [{message, <<"Hello  ", Username/binary>>},
          {auth, true}
         ]};
index(_Req) ->
    {ok, [{message, "Please login!"},
          {auth, false}
         ]}.

login_view(_Req) ->
    {ok, [], #{view => login}}.

login(Req) ->
    reply(gs_auth:login(Req)).

courses_view(#{auth_data := #{auth := true}}) ->
    Cs = gs_lib:enumerate(0, [unicode:characters_to_list(C) || C <- golf_stat:courses()]),
    {ok, [{courses, Cs}], #{view => courses}}.

course_view(#{bindings := #{<<"courseId">> := IdStr}}) ->
    reply(golf_stat:course(to_integer(IdStr))).

add_course_view(#{auth_data := #{auth := true}}) ->
    {ok, [], #{view => add_course}};
add_course_view(_) ->
    {status, 401}.

add_user_round_view(#{auth_data := #{auth := true}}) ->
    Courses = golf_stat:courses(),
    SelStrings = gs_lib:enumerate(0, [unicode:characters_to_list(C) || C <- Courses]),
    {ok, [{courses, SelStrings}], #{view => add_user_round}};
add_user_round_view(_) ->
    {status, 401}.

user_stats_string_view(#{auth_data := #{auth := true, username := User}}) ->
    {ok, [Def|_] = Sels} = golf_stat:user_round_selection(User),
    SelStrings = gs_lib:enumerate(0, [unicode:characters_to_list(C) || C <- Sels]),
    {ok, String} = golf_stat:user_stats_string(User, Def),
    {ok,
     [{selections, SelStrings}, {default, unicode:characters_to_list(String)}],
     #{view => view_string_stats}};
user_stats_string_view(_) ->
    {status, 401}.

user_stats_diagram_view(#{auth_data := #{auth := true, username := User}}) ->
    {ok, [Def|_] = Sels} = golf_stat:user_round_selection(User),
    SelStrings = gs_lib:enumerate(0, [unicode:characters_to_list(C) || C <- Sels]),
    {ok, {_, Data}} = golf_stat:user_diagram_data(User, Def),
    Ids = ["chart" ++ integer_to_list(No) || {No, _} <- gs_lib:enumerate(1, Data)],
    Titels = ["Statistics", "Scores", "Long game", "Short game",
              "Putting", "Number of putts", "Hole Stats"],
    {ok, [{selections, SelStrings},  {ids, lists:zip(Ids, Titels)}],  #{view => view_diagram_stats}};
user_stats_diagram_view(_) ->
    {status, 401}.

%% Json stuff

get_courses(_Req) ->
    reply({ok, golf_stat:courses()}).

get_course(#{bindings := #{<<"courseId">> := IdStr}}) ->
    Id = to_integer(IdStr),
    reply(golf_stat:course(Id)).

add_course(#{json := Req}) ->
    reply(golf_stat:add_course(json_akeys(Req)));
add_course(_Req) ->
    reply({error, <<"Invalid json">>}).

get_user_selections(#{auth_data := #{username := User}}) ->
    reply(golf_stat:user_round_selection(User));
get_user_selections(_Req) ->
    reply({error, <<>>}).

get_stats_string(#{auth_data := #{username := User}, json := #{<<"selection">> := Sel}}) ->
    reply(golf_stat:user_stats_string(User, Sel));
get_stats_string(#{auth_data := #{username := User}, bindings := #{<<"nr">> := SelNr}}) ->
    {ok, Sels} = golf_stat:user_round_selection(User),
    Sel = lists:nth(binary_to_integer(SelNr)+1, Sels),
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
get_stats_diagram(#{auth_data := #{username := User}, bindings := #{<<"nr">> := SelNr}}) ->
    {ok, Sels} = golf_stat:user_round_selection(User),
    Sel = lists:nth(binary_to_integer(SelNr)+1, Sels),
    case golf_stat:user_diagram_data(User, Sel) of
        {ok, DiagramData} ->
            DataSets = setup_diagrams(DiagramData),
            reply({ok, DataSets});
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

to_integer(IdStr) ->
    case string:to_integer(IdStr) of
        {Int, <<>>} -> Int;
        _ -> try gs_lib:base64_decode(IdStr) of
                 Course -> Course
             catch _:_ ->
                     IdStr
             end
    end.

%% charts needs data to be transposed into datasets
setup_diagrams({DataSetLabels, DiagramData}) ->
    setup_diagrams(DiagramData, 1, DataSetLabels, []).

setup_diagrams([DD|DDs], Id, DataSetLabels, Acc) ->
    DataSet = setup_dataset(DD, DataSetLabels),
    %% ?DBG("~p~n",[DataSet]),
    Diagram = #{id => <<"chart", (integer_to_binary(Id))/binary>>,
                data => DataSet},
    setup_diagrams(DDs, Id+1, DataSetLabels, [Diagram|Acc]);
setup_diagrams([], _, _, Acc) ->
    lists:reverse(Acc).

setup_dataset(DD, DataSetLabels) ->
    Labels = [Label || #{label := Label} <- DD],
    DLists = [Data || #{data := Data} <- DD],
    Transposed = transpose(DLists, []),
    DS = lists:zipwith(fun dataset/2, DataSetLabels, Transposed, trim),
    #{labels => Labels, datasets => DS}.

dataset(Label, DataList) ->
    #{label => Label,  data => DataList}.

transpose([[]|_], Acc) ->
    lists:reverse(Acc);
transpose(DLists, Acc) ->
    {Hs, Ts} = split_list(DLists, [], []),
    transpose(Ts, [Hs|Acc]).

split_list([[H|T]|Rest], Hs, Ts) ->
    split_list(Rest, [H|Hs], [T|Ts]);
split_list([], Hs, Ts) ->
    {lists:reverse(Hs), lists:reverse(Ts)}.
