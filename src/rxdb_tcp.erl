-module(rxdb_tcp).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-define(TCP_TIMEOUT, 60000). % 1m

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

%% "{\"value\":\"Val1\",\"key\":\"a\",\"a\":\"get\"}"

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, ?TCP_TIMEOUT) of
	{ok, Data} ->
	    Result = rxdb_api:parse(Data, Socket),
	    Transport:send(Socket, Result),
	    loop(Socket, Transport);
	_ ->
	    ok = Transport:close(Socket)
    end.

