-module(couchbaserl_cluster).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(BASE_URL, "http://~s:~w/pools/default/bucketsStreaming/~s").
-define(BOUNDARY, <<"\n\n\n\n">>).

-record(state, {host, port, bucket, http_conn, partial_data= <<"">>}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    Host = "localhost",
    Port = 8091,
    Bucket = "dev",
    Password = "password",
    
    %% start the cluster config stream
    Url = lists:flatten(io_lib:format(?BASE_URL, [Host, Port, Bucket])),
    Auth = "Basic " ++ base64:encode_to_string(Bucket ++ ":" ++ Password),
    Headers = [{"Authorization", Auth}],
    Opts = [{stream, self}, {sync, false}],
    {ok, HttpConn} = httpc:request(get, {Url, Headers}, [], Opts),

    {ok, #state{host=Host, port=Port, bucket=Bucket, http_conn=HttpConn}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_Ref, stream_start, _Headers}}, State) ->
    {noreply, State};
handle_info({http, {_Ref, stream, Data}}, State) ->
    case get_body(Data, State#state.partial_data) of
	{incomplete, PartialData} ->
	    {noreply, State#state{partial_data=PartialData}};
	{complete, CompleteData} ->
	    {noreply, State#state{partial_data= <<"">>}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal API

get_body(<<"">>, Acc) ->
    {incomplete, Acc};
get_body(?BOUNDARY, Acc) ->
    {complete, Acc};
get_body(<<Chunk:4/bytes, Rest/binary>>, Acc) ->
    get_body(Rest, <<Acc/binary, Chunk/binary>>);
get_body(Rest, Acc) ->
    %% TODO: boundary might be split also :/
    get_body(<<"">>, <<Acc/binary, Rest/binary>>).
