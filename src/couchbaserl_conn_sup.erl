-module(couchbaserl_conn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
    {ok, {{simple_one_for_one, 1, 5},
	  [{couchbaserl_conn, {couchbaserl_conn, start_link, Args},
	    permanent, 5000, worker, [couchbaserl_conn]}]}}.
