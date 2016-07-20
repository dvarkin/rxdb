%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_sub_hub).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([sub/2, unsub/1, unsub/2, update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {subscribers}).

%%%===================================================================
%%% API
%%%===================================================================

sub(Key, Client) ->
    gen_server:cast(?SERVER, {sub, Key, Client}).

unsub(Key) ->
    gen_server:cast(?SERVER, {unsub, Key}).

unsub(Key, Client) ->
    gen_server:cast(?SERVER, {unsub, Key, Client}).

update(Key, Value) ->
    gen_server:cast(?SERVER, {update, Key, Value}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Store = ets:new(rxdb_subscribers_store, 
		    [bag, public]),
    {ok, #state{subscribers = Store}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({sub, Key, Client}, #state{subscribers = Store} = State) ->
    ets:insert(Store, {Key, Client}),
    {noreply, State};

handle_cast({unsub, Key}, #state{subscribers = Store} = State) ->
    ets:delete(Store, Key),
    {noreply, State};

handle_cast({unsub, Key, Client}, #state{subscribers = Store} = State) ->
    ets:delete_object(Store, {Key, Client}),
    {noreply, State};

handle_cast({update, Key, Value}, #state{subscribers = Store} = State) ->
    Clients = [C || {_, C} <- ets:lookup(Store, Key)],
    send_updates(Key, Value, Clients),
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    error_logger:error_msg("Unexpected info ~p: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_updates(_Key, _Value, []) ->
    ok;
send_updates(Key, Value, [C | Clients]) ->
    send_to_port(C, Key, Value),
    send_updates(Key, Value, Clients).

send_to_port(Socket, Key, Value) when is_port(Socket) ->
    case erlang:port_info(Socket) of
	undefined ->
	    ok;
	_ ->
	    Message = rxdb_api:make_query(upd, Key, Value),
	    gen_tcp:send(Socket, Message)
	end;
send_to_port(_Socket, _Key, _Value) ->
    %% TODO: GC dead ports
    ok.
