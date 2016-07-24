%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_protocol).

-define(ACTION, <<"action">>).
-define(KEY, <<"key">>).
-define(VALUE, <<"value">>).

%%%% ACTIONS %%%%

-define(GET, <<"get">>).
-define(PUT, <<"put">>).
-define(DEL, <<"del">>).
-define(SUB, <<"sub">>).
-define(UPD, <<"upd">>).
-define(UNSUB, <<"unsub">>).
-define(EXPIRE, <<"expire">>).

%% PROTOCOL
-export([parse/1, parse/2, make_query/2, make_query/3, make_query/4]).

%%%===================================================================
%%% PROTOCOL
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Entry point for RxDB binary protocol. 
%% Parse binary data and call RxDB internal functions.  
%% @spec parse(Data :: binary()) -> binary().
%% @end
%%--------------------------------------------------------------------

-spec parse(Data :: binary()) -> binary().

parse(Data) when is_binary(Data) -> 
    R = jiffy:decode(Data, [return_maps]),
    Result = protocol(R),
    jiffy:encode(Result);

parse(Data) ->
    {error, Data}.

%%--------------------------------------------------------------------
%% @doc Entry point for RxDB binary protocol. 
%% Parse binary data and call RxDB internal functions.  
%% Client - is an client's port, wich made a call. Use for subscribe/unsubscribe.  
%% @spec parse(Data :: binary(), Client :: port()) -> binary().
%% @end
%%--------------------------------------------------------------------

-spec parse(Data :: binary(), Client :: port()) -> binary().

parse(Data, Client) when is_binary(Data) ->
    R = jiffy:decode(Data, [return_maps]),
    Result = protocol(R, Client),
    jiffy:encode(Result);
parse(Data, Client) ->
    {error, {Data, Client}}.


%%--------------------------------------------------------------------
%% @doc Transfrom RxDB response to binary json. 
%% @spec make_query(Action :: get | del, Key :: binary()) -> binary().
%% @end
%%--------------------------------------------------------------------

-spec make_query(Action :: get | del, Key :: binary()) -> binary().

make_query(Action, Key) ->
    jiffy:encode(#{?ACTION => Action, ?KEY => Key}).

-spec make_query(Action :: put | upd, Key :: binary(), Value :: binary()) -> binary().

make_query(Action, Key, Value) ->
    jiffy:encode(#{?ACTION => Action, ?KEY => Key, ?VALUE => Value}).

make_query(Action, Key, Value, Expire) when Expire > 0 ->
    jiffy:encode(#{?ACTION => Action, ?KEY => Key, ?VALUE => Value, ?EXPIRE => Expire});

make_query(Action, Key, Value, _Expire) ->
    make_query(Action, Key, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Match decoded map and call internal RxDB functions. 
%% This function use for call GET/PUT/DEL functions
%% @private
%% @spec protocol(Data :: map()) -> binary().
%% @end
%%--------------------------------------------------------------------

-spec protocol(Data :: map()) -> binary().

%% GET
protocol(#{?ACTION := ?GET, ?KEY := Key}) ->
    case rxdb:get(Key) of
	[{Key, Value}] -> 
	    #{?KEY => Key, ?VALUE => Value};
	Result -> Result
    end;

protocol(#{?ACTION := ?PUT, ?KEY := Key, ?VALUE := Value, ?EXPIRE := Expire}) ->
    Result =  rxdb:put(Key, Value, Expire),
    rxdb_tools:protocol_error_handler(Result);

%% PUT

protocol(#{?ACTION := ?PUT, ?KEY := Key, ?VALUE := Value}) ->
    Result =  rxdb:put(Key, Value),
    rxdb_tools:protocol_error_handler(Result);

%% DEL
protocol(#{?ACTION := ?DEL, ?KEY := Key}) ->
    Result = rxdb:del(Key),
    rxdb_tools:protocol_error_handler(Result);

protocol(Err) ->
    error_logger:error_msg("PROTOCOL Error: unsupported message ~p~n", [Err]),
    <<"Unupported operation">>.

%%--------------------------------------------------------------------
%% @doc Match decoded map and call internal RxDB functions. 
%% This function use for call SUB/UNSUB functions
%% @private
%% @spec protocol(Data :: map(), Client :: port()) -> binary().
%% @end
%%--------------------------------------------------------------------

-spec protocol(Data :: map(), Client :: port()) -> binary().

%% SUB
protocol(#{?ACTION := ?SUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    rxdb_tools:protocol_error_handler(Result);

%% UNSUB
protocol(#{?ACTION := ?UNSUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    rxdb_tools:protocol_error_handler(Result);

protocol(Other, _Client) ->
    protocol(Other).


