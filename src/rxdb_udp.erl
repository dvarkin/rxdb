%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_udp).

-behaviour(gen_server).

%% API
-export([start_link/1, client/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: port()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) when Port > 0 ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin}, State) ->
    Result = rxdb_api:parse(Bin),
    gen_udp:send(Socket, Host, Port, Result),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -module(rxdb_udp).

%% -export([start/0, client/1]).

%% start() ->
%%     spawn(fun() -> server(4444) end).

%% server(Port) ->
%%     {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
%%     io:format("UDP server opened socket:~p~n",[Socket]),
%%     loop(Socket).

%% loop(Socket) ->
%%     inet:setopts(Socket, [{active, once}]),
%%     receive
%%         {udp, Socket, Host, Port, Bin} ->
%% 	    Result = rxdb_api:parse(Bin),
%%             gen_udp:send(Socket, Host, Port, Result),
%%             loop(Socket)
%%     end.

% Client code
client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, "localhost", 4444, N),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    io:format("client received:~p~n",[Bin])
            after 2000 ->
                    0
            end,
    gen_udp:close(Socket),
    Value.
