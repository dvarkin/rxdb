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

-type rxdb_key() :: binary().
-type rxdb_value() :: binary(). 

%% RXDB API
-export([get/1, put/2, del/1]).

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

get(KeyID) when is_binary(KeyID) ->
    gen_server:call(?SERVER, {get, KeyID});
get(KeyID) ->
    {error, KeyID}.

-spec put(KeyID :: rxdb_key(), Value :: rxdb_value()) -> ok | {error, {term(), term()}}.

put(KeyID, Value) when is_binary(KeyID) andalso is_binary(Value) ->
    gen_server:cast(?SERVER, {put, KeyID, Value});
put(KeyID, Value) ->
    {error, {KeyID, Value}}.

-spec del(KeyID :: rxdb_key()) -> ok | {error, term()}.

del(KeyID) when is_binary(KeyID) ->
    gen_server:cast(?SERVER, {del, KeyID});
del(KeyID) ->
    {error, KeyID}.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Store = ets:new(rxdb_ets_store, 
		    [set, protected]),
    {ok, #state{store = Store}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, KeyID}, From, #state{store = Store} = State) ->
    %% unnecessary here, but nice perfomance trick. 
    spawn(fun() -> gen_server:reply(From, ets:lookup(Store, KeyID)) end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({put, KeyID, Value}, #state{store = Store} = State) ->
    ets:insert(Store, {KeyID, Value}),
    {noreply, State};

handle_cast({del, KeyID}, #state{store = Store} = State) ->
    ets:delete(Store, KeyID),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
