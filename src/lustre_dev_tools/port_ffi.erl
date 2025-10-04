-module(port_ffi).

-export([start/2, send/2]).

%
start(Bin, Args) ->
    open_port({spawn_executable, Bin}, [binary, exit_status, {args, Args}, hide]).

%
send(Port, Data) ->
    Port ! {self(), {command, Data}}.
