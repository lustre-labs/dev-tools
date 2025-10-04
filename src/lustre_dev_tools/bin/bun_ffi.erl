-module(bun_ffi).

-export([extract/2]).

%
extract(Archive, To) ->
    case zip:unzip(Archive, [{cwd, unicode:characters_to_list(To)}]) of
        {ok, [_, Bin]} ->
            {ok, list_to_binary(Bin)};
        {error, _} ->
            {error, nil}
    end.
