-module(rxdb_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Port = 8080,
    ListenerCount = 100,
    
     %% Trail routes
    
    Trails = trails:trails([rxdb_rest_handler,
			    cowboy_swagger_handler]),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),

    %% http listener

    Listener = cowboy:start_http(rxdb_http_listener, ListenerCount, [{port, Port}],
		      [{env, [{dispatch, Dispatch}]},
		       {compress, true},
		       {timeout, 12000}
		      ]),

    case Listener of 
	{ok, _pid} ->
	    rxdb_sup:start_link();
	{error, Reason} -> {error, Reason}
    end.

stop(_State) ->
    ok = cowboy:stop_listener(rxdb_http_listener).

