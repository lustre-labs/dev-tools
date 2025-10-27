-module(system_ffi).

-export([detect_os/0, detect_arch/0, is_alpine/0, run/3, find/1, exit/1]).

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

run(Cmd, Args, Env) ->
    PortResult =
        try
            {ok,
             open_port({spawn_executable, unicode:characters_to_list(Cmd)},
                       [binary,
                        stderr_to_stdout,
                        stream,
                        in,
                        hide,
                        exit_status,
                        {args, [unicode:characters_to_list(Arg) || Arg <- Args]},
                        {env,
                         [{unicode:characters_to_list(Var), unicode:characters_to_list(Val)}
                          || {Var, Val} <- Env]}])}
        catch
            error:Reason ->
                io:format("Unexpected error: ~p~n", [Reason]),
                {error, <<""/utf8>>}
        end,
    case PortResult of
        {ok, Port} ->
            MonRef = erlang:monitor(port, Port),
            {ExitStatus, Bytes} = run_loop(Port, MonRef, [], 0),
            case ExitStatus of
                0 ->
                    {ok, Bytes};
                _ ->
                    {error, Bytes}
            end;
        _ ->
            PortResult
    end.

run_loop(Port, MonRef, Acc, ExitStatus) ->
    receive
        {Port, {data, <<>>}} ->
            run_loop(Port, MonRef, Acc, ExitStatus);
        {Port, {data, Bytes}} ->
            run_loop(Port, MonRef, [Acc, Bytes], ExitStatus);
        {Port, {exit_status, N}} ->
            run_loop(Port, MonRef, Acc, N);
        {'DOWN', MonRef, _, _, _} ->
            % flush an EXIT signal sent to this process if we trap exits.
            receive
                {'EXIT', Port, _} ->
                    ok
            after 0 ->
                ok
            end,
            {ExitStatus, iolist_to_binary(Acc)}
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
