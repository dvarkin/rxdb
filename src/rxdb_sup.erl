-module(rxdb_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    UDP_PORT = 4444,

    RxDB = #{ id => rxdb,
	      start => {rxdb, start_link, []},
	      restart => permanent,
	      shutdown => 5000, 
	      type => worker,
	      modules => [rxdb]
	    },
    RxUDP = #{id => rxdb_udp,
	      start => {rxdb_udp, start_link, [UDP_PORT]},
	      restart => permanent,
	      shutdown => 5000, 
	      type => worker,
	      modules => [rxdb_udp]
	    },
    Procs = [RxDB, RxUDP],
    {ok, {{one_for_one, 1, 5}, Procs}}.
