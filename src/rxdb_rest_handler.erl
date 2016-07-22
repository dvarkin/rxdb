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

-define(RXDB(Call), 
	case Call of 
	    {error, Error} -> error_logger:error_msg("RXDB error occurred  ~p during the call ~w~n", [Error, ??Call]);
	    RxResult -> RxResult
	end).

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
		description => "Set Value by KeyID. Expire key is optional",
		produces => ["text/plain"],
		parameters => [
			       #{name => <<"key_id">>,
				 description => <<"Uniqe Key ID">>,
				 in => <<"path">>,
				 required => true,
				 type => <<"string">>},
			       #{name => <<"expire">>,
				 description => <<"Expire in Seconds (optional)">>,
				 in => <<"query">>,
				 required => false,
				 type => <<"integer">>},
			       #{name => <<"value">>,
				 description => <<"Value">>,
				 in => <<"body">>,
				 required => true,
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

%% GET
handle_get(Req, State) ->
    {KeyID, _Req1} = cowboy_req:binding(key_id, Req),
    Query = rxdb_protocol:make_query(get, KeyID),
    JSONReq = rxdb_protocol:parse(Query),
    {JSONReq, Req, State}.

%% PUT
handle_put(Req, State) ->
    {KeyID, _} = cowboy_req:binding(key_id, Req),
    {ok, Value, Req1} = cowboy_req:body(Req),
    {Expire, _} = cowboy_req:qs_val(<<"expire">>, Req),
%    error_logger:info_msg("expire: ~p~n", [Expire]),
    Query = rxdb_protocol:make_query(put, KeyID, Value, binary_to_int(Expire)),
    JSONReq = rxdb_protocol:parse(Query),
    Req2 = cowboy_req:set_resp_body(JSONReq, Req1),
    {true, Req2, State}.

%% DELETE
delete_resource(Req, State)->
    {KeyID, _} = cowboy_req:binding(key_id, Req),
    Query = rxdb_protocol:make_query(del, KeyID),
    _JSONReq = rxdb_protocol:parse(Query),
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

%% tools

binary_to_int(Bin) when is_binary(Bin) ->
    try list_to_integer(binary_to_list(Bin)) of
	R -> R
    catch
	_Exception -> undefined
    end.
