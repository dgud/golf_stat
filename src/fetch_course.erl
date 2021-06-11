%%% @author Dan <dgud@Dan>
%%% @copyright (C) 2021, Dan
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2021 by Dan <dgud@Dan>

-module(fetch_course).

-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("golf-export.txt"),
    Decoded = jsone:decode(Bin, [{keys, atom}]),
    %% io:format("Top = ~p~n",[maps:keys(Decoded)]),
    Courses0 = [get_course(Entry) || Entry <- maps:get(details, Decoded)],
    Courses = lists:usort(Courses0),
    io:format(" ~p~n",[hd(Courses)]),
    Encoded = jsone:encode(Courses, [native_utf8, {space, 0}, {indent, 0}]),
    file:write_file("courses.txt", Encoded).

get_course(#{courseSnapshots:=[Course]}) ->
    #{name:= Name, holePars:=Pars0} = Course,
    Pars = [list_to_integer([Char]) || <<Char:8>> <= Pars0],
    Utf8 = case unicode:characters_to_binary(Name) of
               Bin when is_binary(Bin) -> Bin;
               {error, _, _} ->
                   Bin = unicode:characters_to_binary(Name, latin1),
                   true = is_binary(Bin),
                   Bin;
               Else ->
                   io:format("~p ~p~n",[Else, Name]),
                   exit(Else)
           end,
    io:format("~ts~n",[Utf8]),
    #{name=>Utf8, pars=>Pars}.


