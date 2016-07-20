-module(rxdb_tcp).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-define(TCP_TIMEOUT, 120000). % 2m

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, ?TCP_TIMEOUT) of
	{ok, Data} ->
	    R = try rxdb_api:parse(Data, Socket) of
		    Result -> Result
		catch 
		    _ -> jiffy:encode(#{error => <<"Incorrect JSON">>, received => Data})
		end,
	    Transport:send(Socket, R),
	    loop(Socket, Transport);
	_ ->
	    ok = Transport:close(Socket)
    end.

