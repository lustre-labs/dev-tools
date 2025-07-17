-module(system_ffi).

-export([detect_os/0, detect_arch/0, run/1]).

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
