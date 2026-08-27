-module(server_ffi).

-export([network_interfaces/0]).

network_interfaces() ->
    try inet:getifaddrs() of
        {ok, Interfaces} ->
            Output =
                lists:filtermap(fun({Name, Opts}) ->
                                   Addrs = [Addr || {addr, Addr} <- Opts],
                                   Flags = proplists:get_value(flags, Opts, []),
                                   IPv4 = [A || A <- Addrs, tuple_size(A) =:= 4],
                                   IsUp = lists:member(up, Flags),
                                   IsLoopback = lists:member(loopback, Flags),

                                   case {IsUp, IsLoopback, IPv4} of
                                       {true, false, [IPv4Addr | _]} -> {true, {Name, IPv4Addr}};
                                       _ -> false
                                   end
                                end,
                                Interfaces),
            {ok, Output}
    catch
        error:Reason ->
            logger:error("Failed getting network interfaces: ~p", [Reason]),
            {error, nil}
    end.
