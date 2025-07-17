-module(bun_ffi).

-export([resolve/2, extract/2]).

% Resolve the name of the Bun archive we need to fetch based on a user's operating
% system and CPU architecture. This will account for older CPUs that require the
% baseline version of Bun, as well as the musl version for Alpine Linux.
%
resolve(<<"darwin">>, <<"aarch64">>) ->
    {ok, <<"bun-darwin-aarch64">>};
resolve(<<"darwin">>, <<"x64">>) ->
    {ok, <<"bun-darwin-x64">>};
resolve(<<"linux">>, <<"aarch64">>) ->
    case requires_musl() of
        true ->
            {ok, <<"bun-linux-aarch64-musl">>};
        false ->
            {ok, <<"bun-linux-aarch64">>}
    end;
resolve(<<"linux">>, <<"x64">>) ->
    case {requires_baseline(<<"linux">>), requires_musl()} of
        {true, true} ->
            {ok, <<"bun-linux-x64-baseline">>};
        {true, false} ->
            {ok, <<"bun-linux-x64">>};
        {false, true} ->
            {ok, <<"bun-linux-x64-musl">>};
        {false, false} ->
            {ok, <<"bun-linux-x64">>}
    end;
resolve(<<"windows">>, <<"x64">>) ->
    case requires_baseline(<<"windows">>) of
        true ->
            {ok, <<"bun-windows-x64-baseline">>};
        false ->
            {ok, <<"bun-windows-x64">>}
    end;
resolve(_, _) ->
    {error, nil}.

% Detect if the current CPU architecture requires the baseline version of Bun
% because it does not support AVX2 instructions.
%
requires_baseline(<<"linux">>) ->
    os:cmd("cat /proc/cpuinfo | grep avx2") =/= "";
requires_baseline(<<"windows">>) ->
    Command =
        "powershell -Command \"(Add-Type -MemberDefinition '[DllImport(\\\"ke"
        "rnel32.dll\\\")] public static extern bool IsProcessorFeaturePresent"
        "(int ProcessorFeature);' -Name 'Kernel32' -Namespace 'Win32' "
        "-PassThru)::IsProcessorFeaturePresent(40)\"",
    Output = os:cmd(Command),

    string:trim(Output) =/= "True";
requires_baseline(_) ->
    false.

%
requires_musl() ->
    filelib:is_file("/etc/alpine-release").

%
extract(Archive, To) ->
    case zip:unzip(Archive, [{cwd, unicode:characters_to_list(To)}]) of
        {ok, [_, Bin]} ->
            {ok, list_to_binary(Bin)};
        {error, _} ->
            {error, nil}
    end.
