-module(couchbaserl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->	
    Procs = [{couchbaserl_conn_sup,
	      {couchbaserl_conn_sup, start_link, []},
	      permanent, 5000, supervisor, [couchbaserl_conn_sup]},
	     {couchbaserl_cluster,
	      {couchbaserl_cluster, start_link, []},
	      permanent, 5000, worker, [couchbaserl_cluster]}],
    
    {ok, {{one_for_one, 1, 5}, Procs}}.
