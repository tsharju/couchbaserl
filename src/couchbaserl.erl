-module(couchbaserl).

-export([start/0, stop/0]).

%% fetch DB operations
-export([get/1]).

%% store DB operations
-export([set/2, set/3]).
-export([set_with_cas/3, set_with_cas/4]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

get(Key) ->
    {VbucketId, Server} = vbucket:map(Key),
    Conn = couchbaserl_cluster:get_connection(Server),
    gen_server:call(Conn, {get, Key, VbucketId}).

set(Key, Value) ->
    set(Key, Value, 0, 0).

set(Key, Value, Expires) ->
    set(Key, Value, Expires, 0).

set_with_cas(Key, Value, Cas) ->
    set(Key, Value, 0, Cas).

set_with_cas(Key, Value, Expires, Cas) ->
    set(Key, Value, Expires, Cas).

%% internal API

set(Key, Value, Expires, Cas) ->
    {VbucketId, Server} = vbucket:map(Key),
    Conn = couchbaserl_cluster:get_connection(Server),
    gen_server:call(Conn, {set, Key, Value, Expires, Cas, VbucketId}).

