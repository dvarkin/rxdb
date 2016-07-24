%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc Simple UDP server
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_udp).

-behaviour(gen_server).

%% PROTOCOL
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: port()}).

%%%===================================================================
%%% PROTOCOL
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @spec start_link(Port :: pos_integer()) -> {ok, Pid} | ignore | {error, Error}
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
    Result = rxdb_protocol:parse(Bin),
    gen_udp:send(Socket, Host, Port, Result),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
