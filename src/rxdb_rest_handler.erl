-module(rxdb_rest_handler).

-behaviour(trails_handler).

%% RANCH DEFAULTS

-export([ init/3
        , rest_init/2
        , content_types_accepted/2
        , content_types_provided/2
        , forbidden/2
        , resource_exists/2
        ]).


-export([trails/0]).
-export([ allowed_methods/2
        , handle_put/2
        , handle_get/2
	, delete_resource/2
        ]).


trails() ->
    Metadata =
	#{get =>
	      #{tags => ["RxDB REST"],
		description => "Get by Key",
		produces => ["text/plain"],
		parameters => [
			       #{name => <<"key_id">>,
				 description => <<"Uniqe Key ID">>,
				 in => <<"path">>,
				 required => true,
				 type => <<"string">>
				}]
	       },
	  put =>
	      #{tags => ["RxDB REST"],
		description => "Set Value by KeyID",
		produces => ["text/plain"],
		parameters => [
			       #{name => <<"key_id">>,
				 description => <<"Uniqe Key ID">>,
				 in => <<"path">>,
				 required => false,
				 type => <<"string">>},
			       #{name => <<"value">>,
				 description => <<"Value">>,
				 in => <<"body">>,
				 required => false,
				 type => <<"string">>}]
	       },
	  delete =>
	      #{tags => ["RxDB REST"],
		description => "DELETE by Key",
		produces => ["text/plain"],
		parameters => [
			       #{name => <<"key_id">>,
				 description => <<"Uniqe Key ID">>,
				 in => <<"path">>,
				 required => true,
				 type => <<"string">>
				}]
	       }
	 },
    [trails:trail("/rest/[:key_id]", ?MODULE, [], Metadata)].

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% internal
handle_get(Req, State) ->
    {KeyID, _Req1} = cowboy_req:binding(key_id, Req),
    DBReq = rxdb:get(KeyID),
    JSONReq = jiffy:encode({DBReq}),
    {JSONReq, Req, State}.

handle_put(Req, State) ->
    {KeyID, _} = cowboy_req:binding(key_id, Req),
    {ok, Value, Req1} = cowboy_req:body(Req),
    rxdb:put(KeyID, Value),
    Body1 = [<<"value: ">> ,Value, <<" stored by Key: ">> , KeyID],
    Req2 = cowboy_req:set_resp_body(Body1, Req1),
    {true, Req2, State}.

delete_resource(Req, State)->
    {KeyID, _} = cowboy_req:binding(key_id, Req),
    rxdb:del(KeyID),
    {true, Req, State}.

%%% DEFAULTS FOR RANCH

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

content_types_accepted(Req, State) ->
  {[{'*', handle_put}], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.
