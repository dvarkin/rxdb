%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_api).

-define(ACTION, <<"a">>).
-define(KEY, <<"key">>).
-define(VALUE, <<"value">>).

%%%% ACTIONS %%%%

-define(GET, <<"get">>).
-define(PUT, <<"put">>).
-define(DEL, <<"del">>).

%% API
-export([parse/1]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

api(#{?ACTION := ?GET, ?KEY := Key}) ->
    case rxdb:get(Key) of
	[{Key, Value}] -> 
	    #{?KEY => Key, ?VALUE => Value};
	Result -> Result
    end;

api(#{?ACTION := ?PUT, ?KEY := Key, ?VALUE := Value}) ->
    Result =  rxdb:put(Key, Value),
    error_handler(Result);

api(#{?ACTION := ?DEL, ?KEY := Key}) ->
    Result = rxdb:del(Key),
    error_handler(Result).

error_handler(Result) ->
    case Result of
	ok -> 
	    <<"ok">>;
	{error, Error} ->
	    error_logger:error_msg("Error: ~p~n", [Error]),
	    <<"error">>
    end.

