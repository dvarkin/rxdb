%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb).

-behaviour(gen_server).

-type rxdb_key() :: binary() | integer() | string().
-type rxdb_value() :: binary() | integer() | string() | boolean(). 

%% RXDB API
-export([get/1, put/2, put/3, del/1]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {store}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get(KeyID :: rxdb_key()) -> [] | [{rxdb_key(), rxdb_value()}] | {error, term()}.

get(KeyID) ->
    gen_server:call(?SERVER, {get, KeyID}).

-spec put(KeyID :: rxdb_key(), Value :: rxdb_value()) -> ok | {error, {term(), term()}}.

put(KeyID, Value) ->
    gen_server:cast(?SERVER, {put, KeyID, Value}).

-spec put(KeyID :: rxdb_key(), Value :: rxdb_value(), Expire :: pos_integer()) -> ok | {error, {term(), term()}}.

put(KeyID, Value, Expire) when Expire > 0 ->
    gen_server:cast(?SERVER, {put, KeyID, Value, Expire}).

-spec del(KeyID :: rxdb_key()) -> ok | {error, term()}.

del(KeyID) ->
    gen_server:cast(?SERVER, {del, KeyID}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Store = ets:new(rxdb_ets_store, 
		    [set, public]),
    {ok, #state{store = Store}}.

%% GET 

handle_call({get, KeyID}, From, #state{store = Store} = State) ->
    %% unnecessary here, but nice perfomance trick. 
    spawn(fun() ->
		  reply_check_expire(From, Store, KeyID)
%		  gen_server:reply(From, ets:lookup(Store, KeyID)) 
	  end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% PUT

handle_cast({put, KeyID, Value}, #state{store = Store} = State) ->
    ets:insert(Store, {KeyID, Value}),

    %% send updates to subscribers

    rxdb_sub_hub:update(KeyID, Value),
    {noreply, State};

%% PUT with expire

handle_cast({put, KeyID, Value, Expire}, #state{store = Store} = State) ->
    ets:insert(Store, {KeyID, Value, erlang:monotonic_time(seconds) + Expire}),

    %% send updates to subscribers

    rxdb_sub_hub:update(KeyID, Value),
    {noreply, State};

%% Delete Key

handle_cast({del, KeyID}, #state{store = Store} = State) ->
    ets:delete(Store, KeyID),
    %% Remove all subscribers for this key
    rxdb_sub_hub:unsub(KeyID),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reply_check_expire(From, Store, KeyID) ->
    R = ets:lookup(Store, KeyID),
    Reply = case R of 
		[{Key, Value}] -> #{<<"key">> => Key, <<"value">> => Value};
		[{Key, Value, Expire}] -> 
		    case is_expire(Expire) of
			true -> #{};
			false -> #{<<"key">> => Key, <<"value">> =>  Value}
		    end;
		[] -> #{}
	    end,
    gen_server:reply(From, Reply).
	    
-spec is_expire(Expire :: integer()) -> boolean().

is_expire(Expire) ->
    Now = erlang:monotonic_time(seconds),
    Now - Expire > 0.
