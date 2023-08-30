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
    ok.

groups() ->
    [].

all() ->
    [start_and_stop, fetch_courses].

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

    {error, _} = get_request("course/31"),
    {error, _} = get_request("course/foobar"),

    ok = application:stop(golf_stat),
    ok.

%%%%

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
        {ok, {{_,200,_} = Status, SHeader, SBody}} ->
            %% ct:pal("~p: GOT: ~p ~p ~.p~n", [?LINE, Status, SHeader, SBody]),
            {ok, jsone:decode(SBody, [{keys, atom}])};
        {ok, {Status, _, SBody}} ->
            ct:pal("~p: GOT: ~p ~p~n", [?LINE, Status, SBody]),
            {error, SBody}
    end.
