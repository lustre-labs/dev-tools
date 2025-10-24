-module(system_ffi).

-export([detect_os/0, detect_arch/0, is_alpine/0, run/1, find_exec/1, find/1, exit/1]).

detect_os() ->
    case os:type() of
        {win32, _} ->
            <<"win32">>;
        {unix, darwin} ->
            <<"darwin">>;
        {unix, linux} ->
            <<"linux">>;
        {_, Unknown} ->
            atom_to_binary(Unknown, utf8)
    end.

detect_arch() ->
    case erlang:system_info(os_type) of
        {unix, _} ->
            [Arch, _] =
                string:split(
                    erlang:system_info(system_architecture), "-"),
            list_to_binary(Arch);
        {win32, _} ->
            case erlang:system_info(wordsize) of
                4 ->
                    <<"ia32">>;
                8 ->
                    <<"x64">>
            end
    end.

is_alpine() ->
    filelib:is_file("/etc/alpine-release").

find_exec(ExeName) ->
    ExeName1 = unicode:characters_to_list(ExeName),
    case os:find_executable(ExeName1) of
        false ->
            {error, <<"Not found">>};
        FullPath ->
            {ok, unicode:characters_to_binary(FullPath)}
    end.

run(Cmd) ->
    case catch os:cmd(
                   unicode:characters_to_list(Cmd), #{exception_on_failure => true})
    of
        {'EXIT', {{command_failed, Output, _}, _}} ->
            {error, unicode:characters_to_binary(Output)};
        Output when is_binary(Output) ->
            {ok, Output};
        Output when is_list(Output) ->
            {ok, unicode:characters_to_binary(Output)};
        Output ->
            io:format("Unexpected output: ~p~n", [Output]),
            {error, <<"">>}
    end.

exit(Status) ->
    erlang:halt(Status).

find(Cmd) ->
    case os:find_executable(
             unicode:characters_to_list(Cmd))
    of
        false ->
            {error, nil};
        Path when is_list(Path) ->
            {ok, unicode:characters_to_binary(Path)}
    end.
