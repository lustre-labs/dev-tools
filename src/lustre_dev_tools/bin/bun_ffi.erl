-module(bun_ffi).

-export([extract/2]).


extract(Archive, To) ->
    case zip:unzip(Archive, [{cwd, unicode:characters_to_list(To)}]) of
        % Some systems will only report the actual extracted file, like
        % "./.lustre/bin/bun-linux-x64/bun"
        {ok, [Bin]} ->
            {ok, list_to_binary(Bin)};
        % Others (like macOS) will report both the directory and the file
        {ok, [_, Bin]} ->
            {ok, list_to_binary(Bin)};
        {error, _} ->
            {error, nil}
    end.
