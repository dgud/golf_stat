-module(gs_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    inets:start(httpc, [{profile, test0}, {profile, test1}, {profile, testn}], inets),
    Config.

end_per_suite(_Config) ->
    ok.

%% init_per_group(_GroupName, Config) ->
%%     Config.

%% end_per_group(_GroupName, _Config) ->
%%     ok.

init_per_testcase(_TestCase, Config) ->
    SrcDir = proplists:get_value(data_dir, Config),
    DestDir = proplists:get_value(priv_dir, Config),
    os:putenv("GS_DIR", DestDir),
    [{ok, _} = file:copy(filename:join(SrcDir,File), filename:join(DestDir, File))
     || File <- filelib:wildcard("*.json", SrcDir)],
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = application:stop(golf_stat),
    ok.

groups() ->
    [].

all() ->
    [start_and_stop,
     login,
     fetch_courses, add_course,
     fetch_available_stats,

     add_user,
     add_round,
     user_stats_string,
     user_diagram_data,

     user_round_trip
    ].

start_and_stop(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    timer:sleep(1000),
    true = is_pid(whereis(golf_stat)),

    {ok, _} = get_request_raw(test0, ""),

    ok = application:stop(golf_stat),
    false = is_pid(whereis(golf_stat)),
    ok.

login(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    {ok, _} = get_request_raw(test0, "login"),
    {ok, _} = post_raw(test0, "/", "username=test0&password=pwd"),

    {error, <<>>} = post_raw(test1, "/", "username=test1&password=badpwd"),

    {ok, _} = post_raw(test1, "/", "username=test1&password=pwd"),
    {ok, _} = post_raw(test2, "/", "username=testn&password=pwd"),

    {error, <<>>} = post_raw(test_foo, "/", "username=test_foo&password=pwd"),
    ok.

login_user(User) ->
    login_user(User, "pwd").

login_user(User, Pwd) ->
    Res = post_raw(User, "/", "username="++ atom_to_list(User) ++ "&password=" ++ Pwd),
    element(1, Res).

fetch_courses(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    ok = login_user(test0),
    {ok, List} = get_request(test0, "api/json/courses"),
    31 = length(List),

    {ok, #{name := <<"Arlanda Master"/utf8>>, pars := Pars0}} = get_request(test0, "api/json/course/0"),
    18 = length(Pars0),
    {ok, #{name := <<"Åkersberga Golfklubb"/utf8>>, pars := Pars30}} = get_request(test0, "api/json/course/30"),
    18 = length(Pars30),

    RGK = base64:encode_to_string(<<"rättviks golfklubb"/utf8>>, #{mode => urlsafe}),
    {ok, #{name := <<"Rättviks Golfklubb"/utf8>>, pars := ParsN}} = get_request(test0, "api/json/course/" ++ RGK),
    18 = length(ParsN),

    {error, <<"Could not find", _/binary>>} = get_request(test0, "api/json/course/31"),
    {error, <<"Could not find", _/binary>>} = get_request(test0, "api/json/course/foobar"),

    FooBar = base64:encode_to_string(<<"foobar"/utf8>>),
    {error, <<"Could not find", _/binary>>} = get_request(test0, "api/json/course/" ++ FooBar),

    [{ok, _} = get_request(test0, "api/json/course/" ++ base64:encode_to_string(Course)) || Course <- List],

    {error, <<>>} = get_request(testfoo, "api/json/course/1"),
    {error, <<>>} = get_request(test1, "api/json/course/1"),

    ok = application:stop(golf_stat),
    ok.

add_course(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    ok = login_user(test0),

    CourseName = <<"ööfoobar"/utf8>>,

    {ok, OrigCs} = get_request(test0, "api/json/courses"),
    false = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, OrigCs),

    NewCourse = #{name => CourseName,
                  pars => [3,3,3, 4,4,4, 5,5,5, 3,4,5, 3,4,5, 3,4,5]
                 },
    {ok, NewCs} = post(test0, "api/json/add_course", NewCourse),
    {ok, NewCs} = get_request(test0, "api/json/courses"),

    true = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, NewCs),
    {ok, NewCourse} = get_request(test0, "api/json/course/" ++ base64:encode_to_string(CourseName)),

    %% Error cases
    {error, <<"Already exists">>} = post(test0, "api/json/add_course", NewCourse),
    {error, <<"Bad course data">>} = post(test0, "api/json/add_course", #{name => <<"">>, pars => maps:get(pars, NewCourse)}),
    {error, <<"Bad course data">>} = post(test0, "api/json/add_course", #{name => <<"bazzo">>, pars => <<"hej">>}),
    {error, <<"Bad course data">>} = post(test0, "api/json/add_course", #{name => <<"bazzo">>, pars => [<<"hej">>]}),

    {error, <<"Invalid json">>} = post_raw(test0, "api/json/add_course", "fooobarish"),

    {error, <<>>} = post(test1, "api/json/add_course", NewCourse),  %% User not logged in should fail
    {error, <<>>} = post(testfoo, "api/json/add_course", NewCourse),  %% User dont exist should fail

    ok = application:stop(golf_stat),
    ok.

fetch_available_stats(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    ok = login_user(test0),
    {ok, Menus0} = get_request(test0, "api/json/user/selections"),
    6 = length(Menus0),

    ok = login_user(test1),
    {ok, Menus1} = get_request(test1, "api/json/user/selections"),
    io:format("~p~n", [Menus1]),
    4 = length(Menus1),
    [<<"All Rounds">> | _] = Menus1,

    ok = login_user(testn),
    {ok, MenusN} = get_request(testn, "api/json/user/selections"),
    io:format("~p~n", [MenusN]),
    65 = length(MenusN),

    [] = lists:filter(fun(Name) -> not is_binary(Name) end, MenusN),

    {error, <<>>} = get_request(testfoo, "api/json/user/selections"),  %% User dont exist should fail
    ok.

add_user(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    Request = #{user => <<"tester">>, pwd => <<"foobar">>},
    ok = login_user(test0),

    {ok, <<"">>} = post(test0, "api/json/user/add_user", Request),
    {error, <<"User already exists", _/binary>>} = post(test0,"api/json/user/add_user",  Request#{user => <<"Tester">>}),

    ok.

add_round(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    Round = #{date => <<"2023-12-24T10:00:00Z">>,
              course => <<"Dunbar Golf Club">>,
              holes =>
                  [#{no => 1,
                     par => 5,
                     shots => [#{good =>1, club => <<"drive">>},
                               #{bad =>1, club => <<"woods">>},
                               #{perfect =>1, club => <<"short putt">>}
                              ]
                    },
                   #{no => 2,
                     par => 4,
                     shots => [#{good =>1, club => <<"iron">>},
                               #{bad =>1, club => <<"drop">>},
                               #{perfect =>1, club => <<"medium putt">>}]
                    },
                   #{no => 3,
                     par => 3,
                     shots => [#{good =>1, club => <<"wedge">>},
                               #{bad =>1, club => <<"pitch">>},
                               #{perfect =>1, club => <<"long putt">>}]
                    },
                   #{no => 4,
                     par => 3,
                     shots => [#{good =>1, club => <<"drive">>},
                               #{bad =>1, club => <<"bunker">>},
                               #{perfect =>1, club => <<"chip">>}]
                    }
                  ]
             },

    AddRound = "api/json/user/add_round",
    ok = login_user(test1),
    {ok, NewMenuStr} = post(test1, AddRound, Round),
    <<"Dunbar Golf Club 2023-12-24">> = NewMenuStr,

    %% Error cases
    {error, <<"Bad round data">>} = post(test1, AddRound, maps:remove(date, Round)),
    {error, <<"Bad round data">>} = post(test1, AddRound, maps:remove(course, Round)),
    {error, <<"Bad round data">>} = post(test1, AddRound, maps:remove(holes, Round)),
    {error, <<"Bad round data">>} = post(test1, AddRound, Round#{course := <<"bad course">>}),

    BadHole1 = #{no => 1, par => 2, shots => [#{good => 1, club => <<"drive">>}]},
    BadHole2 = #{no => <<"1">>, par => 3, shots => [#{good => 1, club => <<"drive">>}]},
    BadHole3 = #{no => 1, par => 3, shots => [#{good => 2, club => <<"drive">>}]},
    BadHole4 = #{no => 1, par => 3, shots => [#{good => 1, club => <<"no_club">>}]},
    BadHole5 = #{no => 1, par => 3, shots => [#{foo => 1, club => <<"drive">>}]},
    BadHole6 = #{no => 1, par => 3, shots => [#{foo => 1, clubs => <<"drive">>}]},

    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole1]}),
    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole2]}),
    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole3]}),
    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole4]}),
    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole5]}),
    {error, <<"Bad hole data", _/binary>>} = post(test1, AddRound, Round#{holes := [BadHole6]}),


    {error, <<>>} = post(test0, AddRound, Round), %% Not logged in

    ok.

user_stats_string(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    ok = login_user(testn),
    {ok, MenusN} = get_request(testn, "api/json/user/selections"),
    [Menu1, Menu2, _, _, Menu3| _] = MenusN,
    Menu4 = lists:last(MenusN),

    StatsPath = "api/json/user/stats_string",
    {ok, Stat1} = post(testn, StatsPath, #{selection => Menu1}),
    true = is_binary(Stat1),
    ct:log("~ts", [Stat1]),

    {ok, Stat2} = post(testn, StatsPath, #{selection => Menu2}),
    ct:log("~ts", [Stat2]),

    {ok, Stat3} = post(testn, StatsPath, #{selection => Menu3}),
    ct:log("~ts", [Stat3]),

    {ok, Stat4} = post(testn, StatsPath, #{selection => Menu4}),
    ct:log("~ts", [Stat4]),

    _ = [{ok, _} = post(testn, StatsPath, #{selection => Menu})
         || Menu <- MenusN],


    {error, <<"Invalid selection", _/binary>>} = post(testn, StatsPath, #{}),
    {error, <<"Bad selection", _/binary>>} = post(testn, StatsPath, #{selection => "12"}),
    {error, <<"No such round", _/binary>>} = post(testn, StatsPath, #{selection => <<"foo">>}),

    {error, <<>>} = post(test1, StatsPath, #{selection => Menu4}),  %% Not logged in
    ok.

user_diagram_data(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    Users = [test0, test1, testn],
    [ok = login_user(User) || User <- Users],

    DiagramPath = "api/json/user/stats_diagram",
    {ok, MenusN} = get_request(testn,  "api/json/user/selections"),
    [Menu1 | _] = MenusN,

    {ok, #{labels := Lbs1, diagrams := Data1} = Stat1} =
        post(testn, DiagramPath, #{selection => Menu1}),
    io:format("Diagram testn Menu1:~n ~tp~n", [Stat1]),
    [] = lists:filter(fun(Name) -> not is_binary(Name) end, Lbs1),
    [ [ (is_binary(Lbl) andalso is_list(Data)) orelse ct:fail({Lbl,Data})
        || #{label := Lbl, data := Data} <- L1 ] || L1 <- Data1],

    TestUser = fun(TUser) ->
                       {ok, Menus} = get_request(TUser,  "api/json/user/selections"),
                       [{ok, #{labels := _, diagrams := _}} =
                            post(TUser, DiagramPath, #{selection => Menu})
                        || Menu <- Menus]
               end,
    [TestUser(TUser) || TUser <- Users],

    {error, <<"Invalid selection", _/binary>>} = post(testn, DiagramPath, #{}),
    {error, <<"No such round", _/binary>>} = post(testn, DiagramPath, #{selection => <<"foo">>}),
    {error, <<"Bad selection", _/binary>>} = post(testn, DiagramPath, #{selection => "17"}),

    {error, <<>>} = post(testfoo, DiagramPath,  #{selection => Menu1}),
    ok.

user_round_trip(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    ok = login_user(test0),

    Request = #{user => <<"new_user">>, pwd => <<"foobar">>},
    {ok, <<"">>} = post(test0, "api/json/user/add_user", Request),

    ok = login_user(new_user, "foobar"),
    {ok, Menus0} = get_request(new_user, "api/json/user/selections"),
    ct:log("Hmm: ~p~n", [Menus0]),
    Menu0 = lists:nth(2, Menus0),
    {ok, <<"No stats available">>} = post(new_user, "api/json/user/stats_string", #{selection => Menu0}),
    {ok, #{labels := [], diagrams := [[#{data := [+0.0], label := <<"gir">>}|_], _, _, _, _, _, _]}} =
        post(new_user, "api/json/user/stats_diagram", #{selection => Menu0}),

    Round = #{date => <<"2023-12-24T10:00:00Z">>,
              course => <<"Dunbar Golf Club">>,
              holes =>
                  [#{no => 1,
                     par => 5,
                     shots => [#{good =>1, club => <<"drive">>},
                               #{bad =>1, club => <<"woods">>},
                               #{perfect =>1, club => <<"short putt">>}
                              ]
                    },
                   #{no => 2,
                     par => 4,
                     shots => [#{good =>1, club => <<"iron">>},
                               #{bad =>1, club => <<"drop">>},
                               #{perfect =>1, club => <<"medium putt">>}]
                    },
                   #{no => 3,
                     par => 3,
                     shots => [#{good =>1, club => <<"wedge">>},
                               #{bad =>1, club => <<"pitch">>},
                               #{perfect =>1, club => <<"long putt">>}]
                    },
                   #{no => 4,
                     par => 3,
                     shots => [#{good =>1, club => <<"drive">>},
                               #{bad =>1, club => <<"bunker">>},
                               #{perfect =>1, club => <<"chip">>}]
                    }
                  ]
             },

    {ok, NewMenuStr} = post(new_user, "api/json/user/add_round", Round),
    ct:pal("MenuStr: ~s~n", [NewMenuStr]),
    {ok, Menus1} = get_request(new_user, "api/json/user/selections"),
    ct:pal("MenuStr: ~p~n", [Menus1]),
    NewMenuStr = lists:last(Menus1),

    {ok, <<"Dunbar Golf", _/binary>>} =
        post(new_user, "api/json/user/stats_string", #{selection => NewMenuStr}),

    {ok, #{labels := [<<"24/12(1)">>], diagrams := [[#{data := [2.0], label := <<"gir">>}|_], _, _, _, _, _, _]}} =
        post(new_user, "api/json/user/stats_diagram", #{selection => NewMenuStr}),

    ok.

%%%%  HELPERS %%%

url() ->
    "http://localhost:8080/".

get_request(User, Url0) ->
    case get_request_raw(User, Url0) of
        {ok, SBody} ->
            {ok, jsone:decode(SBody, [{keys, atom}])};
        Err ->
            Err
    end.

get_request_raw(User, Url0) ->
    Method = get,
    Url = url() ++ Url0,
    Header = [cookie(User)],
    HTTPOptions = [],
    Options = [{body_format, binary}],
    case httpc:request(Method, {Url, Header}, HTTPOptions, Options) of
        {ok, {{_,200,_} = _Status, _SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, SBody};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.

post(User, Url0, Msg) ->
    case post_raw(User, Url0, "application/json", gs_lib:json_encode(Msg)) of
        {ok, SBody} when SBody =/= <<>> ->
            {ok, jsone:decode(SBody, [{keys, atom}])};
        Res ->
            Res
    end.

post_raw(User, Url0, Body) ->
    post_raw(User, Url0, "application/x-www-form-urlencoded", Body).

post_raw(User, Url0, Type, Body) ->
    Method = post,
    Url = url() ++ Url0,
    Header = [cookie(User)],
    HTTPOptions = [],
    Options = [{body_format, binary}],
    case httpc:request(Method, {Url, Header, Type, Body}, HTTPOptions, Options) of
        {ok, {{_,200,_} = _Status, _SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, SBody};
        {ok, {{_,201,_} = _Status, _SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, SBody};
        {ok, {{_,204,_} = _Status, _SHeader, <<>>}} ->
            {ok, <<>>};
        {ok, {{_,204,_} = _Status, _SHeader, SBody}} ->
            {ok, SBody};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.

cookie(User) ->
    {"cookie", "session_id=" ++ atom_to_list(User)}.
