%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_api).

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

%% API
-export([parse/1, parse/2, make_query/2, make_query/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(Data :: binary()) -> binary().

parse(Data) when is_binary(Data) -> 
    R = jiffy:decode(Data, [return_maps]),
    Result = api(R),
    jiffy:encode(Result);

parse(Data) ->
    {error, Data}.

parse(Data, Client) when is_binary(Data) ->
    R = jiffy:decode(Data, [return_maps]),
    Result = api(R, Client),
    jiffy:encode(Result);
parse(Data, Client) ->
    {error, {Data, Client}}.


-spec make_query(Action :: get | del, Key :: binary()) -> binary().

make_query(Action, Key) ->
    jiffy:encode(#{?ACTION => Action, ?KEY => Key}).

-spec make_query(Action :: put | upd, Key :: binary(), Value :: binary()) -> binary().

make_query(Action, Key, Value) ->
    jiffy:encode(#{?ACTION => Action, ?KEY => Key, ?VALUE => Value}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec api(Data :: map()) -> binary().

%% GET
api(#{?ACTION := ?GET, ?KEY := Key}) ->
    case rxdb:get(Key) of
	[{Key, Value}] -> 
	    #{?KEY => Key, ?VALUE => Value};
	Result -> Result
    end;

%% PUT
api(#{?ACTION := ?PUT, ?KEY := Key, ?VALUE := Value}) ->
    Result =  rxdb:put(Key, Value),
    error_handler(Result);

%% DEL
api(#{?ACTION := ?DEL, ?KEY := Key}) ->
    Result = rxdb:del(Key),
    error_handler(Result);

api(Err) ->
    error_logger:error_msg("API Error: unsupported message ~p~n", [Err]),
    <<"Unupported operation">>.

-spec api(Data :: map(), Client :: port()) -> binary().

%% SUB
api(#{?ACTION := ?SUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    error_handler(Result);

%% UNSUB
api(#{?ACTION := ?UNSUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    error_handler(Result);

api(Other, _Client) ->
    api(Other).


-spec error_handler(Result :: ok | {error, term()}) -> binary().

error_handler(Result) ->
    case Result of
	ok -> 
	    ok;
	{error, Error} ->
	    error_logger:error_msg("Error: ~p~n", [Error]),
	    error
    end.

