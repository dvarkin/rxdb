%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_tools).

%% API
-export([protocol_error_handler/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Convinient error handler. 
%% @spec error_handler(Result :: ok | {error, term()}) -> ok | error.
%% @end
%%--------------------------------------------------------------------

-spec protocol_error_handler(Result :: ok | {error, term()}) -> ok | error.

protocol_error_handler(Result) ->
    case Result of
	ok -> 
	    ok;
	{error, Error} ->
	    error_logger:error_msg("Error: ~p~n", [Error]),
	    error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
