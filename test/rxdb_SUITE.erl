%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(rxdb_SUITE).

-compile(export_all).

-define(PUT, <<"{\"value\":\"Val\",\"key\":\"a\",\"action\":\"put\"}">>).
-define(PUT1, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"put\"}">>).
-define(GET, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"get\"}">>).
-define(DEL, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"del\"}">>).
-define(SUB, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"sub\"}">>).
-define(UPD, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"upd\"}">>).
-define(UNSUB, <<"{\"value\":\"Val1\",\"key\":\"a\",\"action\":\"unsub\"}">>).
-define(OK, <<"\"ok\"">>).


-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(mixer),
    ok = application:start(jiffy),
    ok = application:start(trails),
    ok = application:start(cowboy_swagger),
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(rxdb),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(rxdb).

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [binary_test_case, 
     binary_protocol_test_case, 
     udp_protocol_test_case, 
     tcp_protocol_test_case,
     tcp_sub_protocol_test_case
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

binary_test_case(_Config) ->
    Key = <<"a">>,
    Value = <<"1">>,
    ok = rxdb:put(Key, Value),
    [{Key, Value}] = rxdb:get(Key),
    ok = rxdb:del(Key),
    [] = rxdb:get(Key),
    ok.

binary_protocol_test_case(_Config) ->
    Empty = <<"[]">>,
    ?OK = rxdb_api:parse(?PUT),
    <<"{\"value\":\"Val\",\"key\":\"a\"}">> = rxdb_api:parse(?GET),
    ?OK = rxdb_api:parse(?DEL),
    Empty = rxdb_api:parse(?GET),
    ?OK = rxdb_api:parse(?SUB, port),
    ?OK = rxdb_api:parse(?UNSUB, port),
    ok.

udp_protocol_test_case(_Config) ->
    Empty = <<"[]">>,
    ?OK = rxdb_raw_client:udp(?PUT),
    <<"{\"value\":\"Val\",\"key\":\"a\"}">> = rxdb_raw_client:udp(?GET),
    ?OK = rxdb_raw_client:udp(?DEL),
    Empty = rxdb_raw_client:udp(?GET),
    ok.

tcp_protocol_test_case(_Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),
    _Empty = <<"[]">>,
    gen_tcp:send(Sock, ?PUT),
    ?OK = recv(Sock),
    gen_tcp:send(Sock, ?GET),
    <<"{\"value\":\"Val\",\"key\":\"a\"}">> = recv(Sock),
    gen_tcp:send(Sock, ?DEL),
    ?OK = recv(Sock),
    gen_tcp:close(Sock).

tcp_sub_protocol_test_case(_Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),
    {ok, SenderSock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),

    gen_tcp:send(Sock, ?PUT),
    ?OK = recv(Sock),
    gen_tcp:send(Sock, ?SUB),
    ?OK = recv(Sock),
    gen_tcp:send(SenderSock, ?PUT1),

    %% receive via subscribe

    ?UPD = recv(Sock),
    gen_tcp:send(Sock, ?DEL),
    ?OK = recv(Sock),
    gen_tcp:send(Sock, ?UNSUB),
    ?OK = recv(Sock),
    gen_tcp:close(Sock).


recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            Data;
        {error, closed} -> 
            {error, closed}
    end.


