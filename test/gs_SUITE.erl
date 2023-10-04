-module(gs_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:start(inets),
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
    [start_and_stop, fetch_courses, add_course,
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

    {ok, <<"Hello Text Caller">>} = get_request(""),

    ok = application:stop(golf_stat),
    false = is_pid(whereis(golf_stat)),
    ok.

fetch_courses(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    {ok, List} = get_request("courses"),
    31 = length(List),

    {ok, #{name := <<"Arlanda Master"/utf8>>, pars := Pars0}} = get_request("course/0"),
    18 = length(Pars0),
    {ok, #{name := <<"Åkersberga Golfklubb"/utf8>>, pars := Pars30}} = get_request("course/30"),
    18 = length(Pars30),

    RGK = base64:encode_to_string(<<"rättviks golfklubb"/utf8>>, #{mode => urlsafe}),
    {ok, #{name := <<"Rättviks Golfklubb"/utf8>>, pars := ParsN}} = get_request("course/" ++ RGK),
    18 = length(ParsN),

    {error, <<"Could not find", _/binary>>} = get_request("course/31"),
    {error, <<"Bad format", _/binary>>} = get_request("course/foobar"),

    FooBar = base64:encode_to_string(<<"foobar"/utf8>>),
    {error, <<"Could not find", _/binary>>} = get_request("course/" ++ FooBar),

    [{ok, _} = get_request("course/" ++ base64:encode_to_string(Course)) || Course <- List],

    ok = application:stop(golf_stat),
    ok.

add_course(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    CourseName = <<"ööfoobar"/utf8>>,

    {ok, OrigCs} = get_request("courses"),
    false = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, OrigCs),

    NewCourse = #{name => CourseName,
                  pars => [3,3,3, 4,4,4, 5,5,5, 3,4,5, 3,4,5, 3,4,5]
                 },
    {ok, NewCs} = post("add_course", NewCourse),
    {ok, NewCs} = get_request("courses"),

    true = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, NewCs),
    {ok, NewCourse} = get_request("course/" ++ base64:encode_to_string(CourseName)),

    %% Error cases
    {error, <<"Already exists">>} = post("add_course", NewCourse),
    {error, <<"Bad course data">>} = post("add_course", #{name => <<"">>, pars => maps:get(pars, NewCourse)}),
    {error, <<"Bad course data">>} = post("add_course", #{name => <<"bazzo">>, pars => <<"hej">>}),
    {error, <<"Bad course data">>} = post("add_course", #{name => <<"bazzo">>, pars => [<<"hej">>]}),

    {error, <<"Invalid json">>} = post_plain("add_course", "fooobarish"),
    ok = application:stop(golf_stat),
    ok.

fetch_available_stats(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    {ok, Menus0} = post("user", #{req => <<"selections">>, user => <<"test0">>}),
    5 = length(Menus0),

    {ok, Menus1} = post("user", #{req => <<"selections">>, user => <<"test1">>}),
    4 = length(Menus1),
    [<<"All Rounds">> | _] = Menus1,

    {ok, MenusN} = post("user", #{req => <<"selections">>, user => <<"testn">>}),
    64 = length(MenusN),

    [] = lists:filter(fun(Name) -> not is_binary(Name) end, MenusN),

    {error, <<"No such user:", _/binary>>} = post("user", #{user => <<"not_exists">>, req => <<"selections">>}),
    {error, <<"Bad request", _/binary>>} = post("user", #{user => <<"not_exists">>, request => <<"no_such_req">>}),
    {error, <<"Unknown Request", _/binary>>} = post("user", #{user => <<"not_exists">>, req => <<"no_such_req">>}),

    ok.

add_user(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    Request = #{req => <<"add_user">>, user => <<"tester">>},
    {ok, <<"">>} = post("user", Request),
    {error, <<"User already exists", _/binary>>} = post("user", Request#{user => <<"Tester">>}),

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

    Request = #{req => <<"add_round">>, user => <<"test1">>, round => Round},
    {ok, NewMenuStr} = post("user", Request),
    <<"Dunbar Golf Club 2023-12-24">> = NewMenuStr,

    %% Error cases
    {error, <<"Bad round data">>} = post("user", Request#{round := maps:remove(date, Round)}),
    {error, <<"Bad round data">>} = post("user", Request#{round := maps:remove(course, Request)}),
    {error, <<"Bad round data">>} = post("user", Request#{round := maps:remove(holes, Request)}),
    {error, <<"Bad round data">>} = post("user", Request#{round := Round#{course := <<"bad course">>}}),

    BadHole1 = #{no => 1, par => 2, shots => [#{good => 1, club => <<"drive">>}]},
    BadHole2 = #{no => <<"1">>, par => 3, shots => [#{good => 1, club => <<"drive">>}]},
    BadHole3 = #{no => 1, par => 3, shots => [#{good => 2, club => <<"drive">>}]},
    BadHole4 = #{no => 1, par => 3, shots => [#{good => 1, club => <<"no_club">>}]},
    BadHole5 = #{no => 1, par => 3, shots => [#{foo => 1, club => <<"drive">>}]},
    BadHole6 = #{no => 1, par => 3, shots => [#{foo => 1, clubs => <<"drive">>}]},

    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole1]}}),
    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole2]}}),
    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole3]}}),
    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole4]}}),
    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole5]}}),
    {error, <<"Bad hole data", _/binary>>} = post("user", Request#{round := Round#{holes := [BadHole6]}}),

    ok.

user_stats_string(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    User = <<"testn">>,
    {ok, MenusN} = post("user", #{req => <<"selections">>, user => User}),
    [Menu1, Menu2, _, _, Menu3| _] = MenusN,
    Menu4 = lists:last(MenusN),

    {ok, Stat1} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu1}),
    true = is_binary(Stat1),
    ct:log("~ts", [Stat1]),

    {ok, Stat2} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu2}),
    ct:log("~ts", [Stat2]),

    {ok, Stat3} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu3}),
    ct:log("~ts", [Stat3]),

    {ok, Stat4} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu4}),
    ct:log("~ts", [Stat4]),

    _ = [{ok, _} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu})
         || Menu <- MenusN],

    {error, <<"Unknown Request", _/binary>>} = post("user", #{user => User, req => <<"stats_string">>}),
    {error, <<"Bad selection", _/binary>>} = post("user", #{user => User, req => <<"stats_string">>, selection => "foo"}),
    {error, <<"No such round", _/binary>>} = post("user", #{user => User, req => <<"stats_string">>, selection => <<"foo">>}),
    {error, <<"No such user", _/binary>>} = post("user", #{user => <<"no_user">>, req => <<"stats_string">>, selection => Menu1}),

    ok.

user_diagram_data(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    User = <<"testn">>,
    {ok, MenusN} = post("user", #{req => <<"selections">>, user => User}),
    [Menu1 | _] = MenusN,

    {ok, #{labels := Lbs1, diagrams := Data1} = Stat1} =
        post("user", #{req => <<"stats_diagram">>, user => User, selection => Menu1}),
    ct:log("~tp", [Stat1]),
    [] = lists:filter(fun(Name) -> not is_binary(Name) end, Lbs1),
    [ [ (is_binary(Lbl) andalso is_list(Data)) orelse ct:fail({Lbl,Data})
        || #{label := Lbl, data := Data} <- L1 ] || L1 <- Data1],

    Users = [<<"test0">>, <<"test1">>, <<"testn">>],

    TestUser = fun(TUser) ->
                       {ok, Menus} = post("user", #{req => <<"selections">>, user => TUser}),
                       [{ok, #{labels := _, diagrams := _}} =
                            post("user", #{user => TUser, req => <<"stats_diagram">>, selection => Menu})
                        || Menu <- Menus]
               end,
    [TestUser(TUser) || TUser <- Users],

    {error, <<"Unknown Request", _/binary>>} = post("user", #{user => User, req => <<"stats_diagram">>}),
    {error, <<"Bad selection", _/binary>>} = post("user", #{user => User, req => <<"stats_diagram">>, selection => "foo"}),
    {error, <<"No such round", _/binary>>} = post("user", #{user => User, req => <<"stats_diagram">>, selection => <<"foo">>}),
    {error, <<"No such user", _/binary>>} = post("user", #{user => <<"no_user">>, req => <<"stats_diagram">>, selection => Menu1}),

    ok.

user_round_trip(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),
    User = <<"new_user">>,
    Request = #{req => <<"add_user">>, user => User},
    {ok, <<"">>} = post("user", Request),

    {ok, Menus0} = post("user", #{req => <<"selections">>, user => User}),
    ct:log("Hmm: ~p~n", [Menus0]),
    Menu0 = lists:nth(2, Menus0),
    {ok, <<"No stats available">>} = post("user", #{req => <<"stats_string">>, user => User, selection => Menu0}),
    {ok, #{labels := [], diagrams := [[#{data := [+0.0], label := <<"gir">>}|_], _, _, _, _, _, _]}} =
        post("user", #{req => <<"stats_diagram">>, user => User, selection => Menu0}),

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

    {ok, NewMenuStr} = post("user", #{req => <<"add_round">>, user => User, round => Round}),
    ct:pal("MenuStr: ~s~n", [NewMenuStr]),
    {ok, Menus1} = post("user", #{req => <<"selections">>, user => User}),
    ct:pal("MenuStr: ~p~n", [Menus1]),
    NewMenuStr = lists:last(Menus1),

    {ok, <<"Dunbar Golf", _/binary>>} =
        post("user", #{req => <<"stats_string">>, user => User, selection => NewMenuStr}),

    {ok, #{labels := [<<"24/12(1)">>], diagrams := [[#{data := [2.0], label := <<"gir">>}|_], _, _, _, _, _, _]}} =
        post("user", #{req => <<"stats_diagram">>, user => User, selection => Menu0}),

    ok.

%%%%  HELPERS %%%

url() ->
    "http://localhost:21137/".

get_request(Url0) ->
    Method = get,
    Url = url() ++ Url0,
    Header = [],
    %% _Type = case Type0 of
    %%            json -> "application/json";
    %%            html -> "text/html"
    %%        end,
    HTTPOptions = [],
    Options = [{body_format, binary}],
    case httpc:request(Method, {Url, Header}, HTTPOptions, Options) of
        {ok, {{_,200,_} = _Status, _SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.

post(Url0, Msg) ->
    post_plain(Url0, gs_lib:json_encode(Msg)).

post_plain(Url0, Body) ->
    Method = post,
    Url = url() ++ Url0,
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [{body_format, binary}],
    case httpc:request(Method, {Url, Header, Type, Body}, HTTPOptions, Options) of
        {ok, {{_,200,_} = _Status, _SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {{_,204,_} = _Status, _SHeader, <<>>}} ->
            {ok, <<>>};
        {ok, {{_,204,_} = _Status, _SHeader, SBody}} ->
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.
