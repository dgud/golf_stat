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
    [start_and_stop, fetch_courses, add_course].

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

    {error, _} = get_request("course/31"),
    {error, _} = get_request("course/foobar"),

    FooBar = base64:encode_to_string(<<"foobar"/utf8>>),
    {error, _} = get_request("course/" ++ FooBar),

    ok = application:stop(golf_stat),
    ok.

add_course(_Config) ->
    {ok, _} = application:ensure_all_started(golf_stat),

    CourseName = <<"foobar"/utf8>>,

    {ok, OrigCs} = get_request("courses"),
    false = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, OrigCs),

    NewCourse = #{name => CourseName,
                  pars => [3,3,3, 4,4,4, 5,5,5, 3,4,5, 3,4,5, 3,4,5]
                 },
    {ok, <<>>} = post("add_course", NewCourse),

    {ok, NewCs} = get_request("courses"),
    true = lists:any(fun(Name) -> string:equal(Name, CourseName, true) end, NewCs),
    {ok, NewCourse} = get_request("course/" ++ base64:encode_to_string(CourseName)),

    %% Error cases
    {error, <<"\"Already exists\"">>} = post("add_course", NewCourse),
    {error, <<"\"Bad course data\"">>} = post("add_course", #{name => <<"">>, pars => maps:get(pars, NewCourse)}),
    {error, <<"\"Bad course data\"">>} = post("add_course", #{name => <<"bazzo">>, pars => <<"hej">>}),
    {error, <<"\"Bad course data\"">>} = post("add_course", #{name => <<"bazzo">>, pars => [<<"hej">>]}),

    {error, <<"\"Invalid json\"">>} = post_plain("add_course", "fooobarish"),
    ok = application:stop(golf_stat),
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
            ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, _Status, _SHeader, SBody]),
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {{_,204,_} = _Status, _SHeader, <<>>}} ->
            {ok, <<>>};
        {ok, {{_,204,_} = _Status, _SHeader, SBody}} ->
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.
