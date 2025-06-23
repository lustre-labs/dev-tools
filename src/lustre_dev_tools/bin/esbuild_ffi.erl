-module(esbuild_ffi).

-export([unzip/1]).

unzip(Zip) ->
    Filepath =
        case os:type() of
            {win32, _} ->
                "package/esbuild.exe";
            _ ->
                "package/bin/esbuild"
        end,

    Result = erl_tar:extract({binary, Zip}, [memory, compressed, {files, [Filepath]}]),

    case Result of
        {ok, [{_, Esbuild}]} ->
            {ok, Esbuild};
        {ok, Res} ->
            {error, Res};
        {error, Err} ->
            {error, Err}
    end.
