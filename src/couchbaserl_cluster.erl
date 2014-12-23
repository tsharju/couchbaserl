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

-record(state, {host, port, bucket, http_conn,
		partial_boundary= <<"">>, partial_data= <<"">>}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    Host = application:get_env(couchbaserl, host, "localhost"),
    Port = application:get_env(couchbaserl, port, 8091),
    Bucket = application:get_env(couchbaserl, bucket, "default"),
    Password = application:get_env(couchbaserl, password, ""),
    
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
    case get_body(Data, State#state.partial_boundary, State#state.partial_data) of
	{partial, PartialBoundary, PartialData} ->
	    {noreply, State#state{partial_boundary=PartialBoundary,
				  partial_data=PartialData}};
	{complete, CompleteData, PartialData} ->
	    ok = vbucket:config_parse(binary_to_list(CompleteData)),
	    {noreply, State#state{partial_data=PartialData}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal API

get_body(Data, <<"\n\n\n\n">>, Acc2) ->
    {complete, Acc2, Data};
get_body(<<"">>, <<"\n\n\n">>, Acc2) ->
    {partial, <<"\n\n\n">>, Acc2};
get_body(<<"">>, <<"\n\n">>, Acc2) ->
    {partial, <<"\n\n">>, Acc2};
get_body(<<"">>, <<"\n">>, Acc2) ->
    {partial, <<"\n">>, Acc2};
get_body(<<"">>, <<"">>, Acc2) ->
    {partial, <<"">>, Acc2};
get_body(<<Byte:1/bytes, Rest/binary>>, <<"\n">>, Acc2) when Byte =/= <<"\n">> ->
    get_body(Rest, <<"">>, <<Acc2/binary, "\n", Byte/binary>>);
get_body(<<Byte:1/bytes, Rest/binary>>, <<"\n\n">>, Acc2) when Byte =/= <<"\n">> ->
    get_body(Rest, <<"">>, <<Acc2/binary, "\n\n", Byte/binary>>);
get_body(<<Byte:1/bytes, Rest/binary>>, <<"\n\n\n">>, Acc2) when Byte =/= <<"\n">> ->
    get_body(Rest, <<"">>, <<Acc2/binary, "\n\n\n", Byte/binary>>);
get_body(<<Byte:1/bytes, Rest/binary>>, Acc1, Acc2) when Byte =:= <<"\n">> ->
    get_body(Rest, <<Acc1/binary, Byte/binary>>, Acc2);
get_body(<<Byte:1/bytes, Rest/binary>>, Acc1, Acc2) ->
    get_body(Rest, Acc1, <<Acc2/binary, Byte/binary>>).
