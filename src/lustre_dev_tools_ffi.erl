-module(lustre_dev_tools_ffi).
-export([
    check_live_reloading/0,
    fs_start_link/2,
    get_cwd/0,
    get_cpu/0,
    get_esbuild/1,
    get_tailwind/1,
    get_os/0,
    otp_version/0,
    unzip_esbuild/1,
    exec/3
]).

otp_version() ->
    Version = erlang:system_info(otp_release),
    list_to_binary(Version).

get_cwd() ->
    case file:get_cwd() of
        {ok, Cwd} -> {ok, list_to_binary(Cwd)};
        {error, Reason} -> {error, Reason}
    end.

get_os() ->
    case os:type() of
        {win32, _} -> <<"win32">>;
        {unix, darwin} -> <<"darwin">>;
        {unix, linux} -> <<"linux">>;
        {_, Unknown} -> atom_to_binary(Unknown, utf8)
    end.

get_cpu() ->
    case erlang:system_info(os_type) of
        {unix, _} ->
            [Arch, _] = string:split(erlang:system_info(system_architecture), "-"),
            list_to_binary(Arch);
        {win32, _} ->
            case erlang:system_info(wordsize) of
                4 -> <<"ia32">>;
                8 -> <<"x64">>
            end
    end.

get_esbuild(Url) ->
    inets:start(),
    ssl:start(),

    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Zip}} -> {ok, Zip};
        {ok, Res} -> {error, Res};
        {error, Err} -> {error, Err}
    end.

get_tailwind(Url) ->
    inets:start(),
    ssl:start(),

    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Bin}} -> {ok, Bin};
        {ok, Res} -> {error, Res};
        {error, Err} -> {error, Err}
    end.

unzip_esbuild(Zip) ->
    Filepath =
        case os:type() of
            {win32, _} -> "package/esbuild.exe";
            _ -> "package/bin/esbuild"
        end,

    Result =
        erl_tar:extract({binary, Zip}, [
            memory, compressed, {files, [Filepath]}
        ]),

    case Result of
        {ok, [{_, Esbuild}]} -> {ok, Esbuild};
        {ok, Res} -> {error, Res};
        {error, Err} -> {error, Err}
    end.

exec(Command, Args, Cwd) ->
    Command_ = binary_to_list(Command),
    Args_ = lists:map(fun(Arg) -> binary_to_list(Arg) end, Args),
    Cwd_ = binary_to_list(Cwd),

    Name = case Command_ of
      "./" ++ _ -> {spawn_executable, Command_};
      "/" ++ _ -> {spawn_executable, Command_};
      _ -> {spawn_executable, os:find_executable(Command_)}
    end,

    Port = open_port(Name, [
        exit_status,
        binary,
        hide,
        stream,
        eof,
        stderr_to_stdout, % We need this to hide the process' stdout
        {args, Args_},
        {cd, Cwd_}
    ]),

    do_exec(Port, []).

do_exec(Port, Acc) ->
    receive
        {Port, {data, Data}} -> do_exec(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
          port_close(Port),
          {ok, list_to_binary(lists:reverse(Acc))};
        {Port, {exit_status, Code}} ->
          port_close(Port),
          {error, {Code, list_to_binary(lists:reverse(Acc))}}
    end.

fs_start_link(Id, Path) ->
    % We temporarily disable all logs when we call `start_link` because we want
    % to be displaying a nicer custom error message and not the logged error
    % coming from `fs`.
    logger:add_primary_filter(discard_all, {fun(_, _) -> stop end, no_extra}),
    Result = fs:start_link(Id, Path),
    logger:remove_primary_filter(discard_all),
    Result.

% This is what the underlying `fs` library does to check if it has support for
% a given os:
%
% https://github.com/5HT/fs/blob/23a5b46b033437a3d69504811ae6c72f7704a78a/src/fs_sup.erl#L18-L46
%
% Sadly the library doesn't expose such a function and just logs any error
% instead of surfacing it as a value, so we have to implement a slightly
% modified version of it to have proper error messages.
check_live_reloading() ->
    Watcher =
        case os:type() of
            {unix, darwin} -> fsevents;
            {unix, linux} -> inotifywait;
            {unix, sunos} -> undefined;
            {unix, _} -> kqueue;
            {win32, nt} -> inotifywait_win32;
            _ -> undefined
        end,

    case Watcher of
        undefined -> {error, no_file_watcher_supported_for_os};
        _ ->
            case Watcher:find_executable() of
                false -> {error, {no_file_watcher_installed, Watcher}};
                _ -> {ok, nil}
            end
    end.
