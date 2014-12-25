-module(couchbaserl).

-include("couchbaserl.hrl").

-export([start/0, stop/0]).

%% fetch DB operations
-export([get/1]).

%% store DB operations
-export([set/2, set/3]).
-export([add/2, add/3]).
-export([replace/2, replace/3]).
-export([set_with_cas/3, set_with_cas/4]).

%% other operations
-export([delete/1, delete_quiet/1]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

get(Key) ->
    operation(?OP_GET, Key, []).

set(Key, Value) ->
    mutate(?OP_SET, Key, Value, 0, 0).

set(Key, Value, Expires) ->
    mutate(?OP_SET, Key, Value, Expires, 0).

set_with_cas(Key, Value, Cas) ->
    mutate(?OP_SET, Key, Value, 0, Cas).

set_with_cas(Key, Value, Expires, Cas) ->
    mutate(?OP_SET, Key, Value, Expires, Cas).

add(Key, Value) ->
    mutate(?OP_ADD, Key, Value, 0, 0).

add(Key, Value, Expires) ->
    mutate(?OP_ADD, Key, Value, Expires, 0).

replace(Key, Value) ->
    mutate(?OP_REPLACE, Key, Value, 0, 0).

replace(Key, Value, Expires) ->
    mutate(?OP_REPLACE, Key, Value, Expires, 0).

delete(Key) ->
    operation(?OP_DELETE, Key, false).

delete_quiet(Key) ->
    operation(?OP_DELETE, Key, true).

%% internal API

operation(Opcode, Key, Opts) ->
    {VbucketId, Server} = vbucket:map(Key),
    Conn = couchbaserl_cluster:get_connection(Server),
    gen_server:call(Conn, {Opcode, Key, VbucketId, Opts}).

mutate(Opcode, Key, Value, Expires, Cas) ->
    {VbucketId, Server} = vbucket:map(Key),
    Conn = couchbaserl_cluster:get_connection(Server),
    gen_server:call(Conn, {Opcode, Key, Value, Expires, Cas, VbucketId}).
