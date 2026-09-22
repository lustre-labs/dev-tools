-module(server_ffi).

-export([network_interfaces/0]).

network_interfaces() ->
    try inet:getifaddrs() of
        {ok, Interfaces} ->
            lists:filtermap(
                fun({Name, Opts}) ->
                    Addrs = [Addr || {addr, Addr} <- Opts],
                    Flags = proplists:get_value(flags, Opts, []),
                    IPv4 = [A || A <- Addrs, tuple_size(A) =:= 4],
                    IsUp = lists:member(up, Flags),
                    IsLoopback = lists:member(loopback, Flags),

                    case {IsUp, IsLoopback, IPv4} of
                        {true, false, [IpV4Addr | _]} -> {true, {Name, IpV4Addr}};
                        _ -> false
                    end
                end,
                Interfaces
            )
    catch
        error:_ ->
            []
    end.
