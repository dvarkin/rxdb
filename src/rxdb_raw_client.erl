%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>

-module(rxdb_raw_client).

-export([udp/1]).

udp(N) when is_binary(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened UDP socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, "localhost", 4444, N),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    io:format("client received:~p~n",[Bin]),
		    Bin
            after 2000 ->
                    0
            end,
    gen_udp:close(Socket),
    Value.


