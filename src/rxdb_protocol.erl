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

-spec parse(Data :: binary()) -> binary().

parse(Data) when is_binary(Data) -> 
    R = jiffy:decode(Data, [return_maps]),
    Result = protocol(R),
    jiffy:encode(Result);

parse(Data) ->
    {error, Data}.

parse(Data, Client) when is_binary(Data) ->
    R = jiffy:decode(Data, [return_maps]),
    Result = protocol(R, Client),
    jiffy:encode(Result);
parse(Data, Client) ->
    {error, {Data, Client}}.


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
    error_handler(Result);

%% PUT

protocol(#{?ACTION := ?PUT, ?KEY := Key, ?VALUE := Value}) ->
    Result =  rxdb:put(Key, Value),
    error_handler(Result);


%% DEL
protocol(#{?ACTION := ?DEL, ?KEY := Key}) ->
    Result = rxdb:del(Key),
    error_handler(Result);

protocol(Err) ->
    error_logger:error_msg("PROTOCOL Error: unsupported message ~p~n", [Err]),
    <<"Unupported operation">>.

-spec protocol(Data :: map(), Client :: port()) -> binary().

%% SUB
protocol(#{?ACTION := ?SUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    error_handler(Result);

%% UNSUB
protocol(#{?ACTION := ?UNSUB, ?KEY := Key}, Client) ->
    Result = rxdb_sub_hub:sub(Key, Client),
    error_handler(Result);

protocol(Other, _Client) ->
    protocol(Other).


-spec error_handler(Result :: ok | {error, term()}) -> ok | error.

error_handler(Result) ->
    case Result of
	ok -> 
	    ok;
	{error, Error} ->
	    error_logger:error_msg("Error: ~p~n", [Error]),
	    error
    end.

