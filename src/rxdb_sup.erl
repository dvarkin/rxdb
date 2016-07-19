-module(rxdb_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RxDB = #{ id => rxdb,
	      start => {rxdb, start_link, []},
	      restart => permanent,
	      shutdown => 5000, 
	      type => worker,
	      modules => [rxdb_ets]
	    },
    Procs = [RxDB],
    {ok, {{one_for_one, 1, 5}, Procs}}.
