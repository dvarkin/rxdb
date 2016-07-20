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

    {ok, _} = cowboy:start_http(rxdb_http_listener, ListenerCount, [{port, Port}],
		      [{env, [{dispatch, Dispatch}]},
		       {compress, true},
		       {timeout, 12000}
		      ]),
    %% tcp listener

    {ok, _}  = ranch:start_listener(rxdb_tcp_listener, 100,
				    ranch_tcp, [{port, 5555}],
				    rxdb_tcp, []
				   ),
    
    %% Start rxdb supervisor

    rxdb_sup:start_link().


stop(_State) ->
    ok = ranch:stop_listener(rxdb_tcp_listener),
    ok = cowboy:stop_listener(rxdb_http_listener).

