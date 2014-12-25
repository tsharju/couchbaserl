-module(couchbaserl_conn).
-behaviour(gen_server).

-include("couchbaserl.hrl").

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(HEADER_LENGTH, 24).

-record(state, {socket}).

%% API.

-spec start_link({string(), integer()}) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% gen_server.
init({Host, Port}) ->
    Opts = [binary, {nodelay, true}, {active, false}, {packet, 0}],
    {ok, Socket} = gen_tcp:connect(Host, Port, Opts),

    %% register this process to gproc
    true = gproc:add_local_name({conn, {Host, Port}}),

    {ok, #state{socket=Socket}}.

handle_call({authenticate, BucketName, Password}, _From, #state{socket=Socket} = State) ->
    %% list SASL mechanisms
    MResp = send_and_receive(Socket, #req{opcode=?OP_SASL_LIST_MECHANISMS}),
    Mechanisms = couchbaserl_auth:decode_mechanisms(MResp#rsp.body),
    
    %% do then auth dance
    case lists:member('CRAM-MD5', Mechanisms) of
	true ->
	    %% request challenge from server
	    CResp = send_and_receive(Socket,
				     #req{opcode=?OP_SASL_AUTHENTICATE, key="CRAM-MD5"}),
	    {auth_continue, AuthChallenge} = {CResp#rsp.status, CResp#rsp.body},
	    AuthResponse = couchbaserl_auth:sasl_cram_md5(AuthChallenge, BucketName,
							  Password),
	    %% send the response
	    Result = send_and_receive(Socket,
				      #req{opcode=?OP_SASL_STEP,
					   key="CRAM-MD5", body=AuthResponse}),
	    case Result#rsp.status of
		success ->
		    {reply, ok, State};
		_ ->
		    {reply, {error, binary_to_list(Result#rsp.body)}, State}
	    end;
	false ->
	    {reply, {error, not_implemented}, State}
    end;
handle_call({?OP_GET, Key, VbucketId, []}, _From, #state{socket=Socket} = State) ->
    Request = #req{opcode=?OP_GET, key=Key, vbucket=VbucketId},
    Response = respond_with_cas_and_body(send_and_receive(Socket, Request)),
    {reply, Response, State};
handle_call({?OP_DELETE, Key, VbucketId, Quiet}, _From, #state{socket=Socket} = State) ->
    Request = #req{opcode=?OP_DELETE, key=Key, vbucket=VbucketId},
    Response = send_and_receive(Socket, Request),
    case {Quiet, Response} of
	{true, _} ->
	    {reply, ok, State};
	{_, R} when R#rsp.status =:= success ->
	    {reply, ok, State};
	{_, R} ->
	    {reply, {error, {R#rsp.status, binary_to_list(R#rsp.body)}}, State}
    end;
handle_call({Opcode, Key, Value, Expires, Cas, VbucketId}, _From,
	    #state{socket=Socket} = State) when Opcode =:= ?OP_SET;
						Opcode =:= ?OP_ADD;
						Opcode =:= ?OP_REPLACE ->
    Extras = <<0:32, Expires:32>>,
    Request = #req{opcode=Opcode, key=Key, body=Value, extras=Extras,
		   vbucket=VbucketId, cas=Cas},
    Response = respond_with_cas(send_and_receive(Socket, Request)),
    {reply, Response, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private API

send_and_receive(Socket, Request) ->
    send_request(Socket, Request),
    receive_response(Socket).

send_request(Socket, Request) ->
    ReqData = encode_request(Request),
    case gen_tcp:send(Socket, ReqData) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

receive_response(Socket) ->
    Response = receive_header(Socket),
    receive_body(Socket, Response).

receive_header(Socket) ->
    case gen_tcp:recv(Socket, ?HEADER_LENGTH) of
	{ok, Header} ->
	    decode_response_header(Header);
	{error, Reason} ->
	    exit(Reason)
    end.

receive_body(_Socket, #rsp{total_body_length=0} = Response) ->
    Response;
receive_body(Socket, #rsp{total_body_length=BodyLen} = Response) ->
    case gen_tcp:recv(Socket, BodyLen) of
	{ok, Body} ->
	    decode_response_body(Response, Body);
	{error, Reason} ->
	    exit(Reason)
    end.

encode_request(#req{key=Key} = R) when is_list(Key) ->
    encode_request(R#req{key=list_to_binary(Key)});
encode_request(#req{body=Body} = R) when is_list(Body) ->
    encode_request(R#req{body=list_to_binary(Body)});
encode_request(Request) when is_record(Request, req) ->
    #req{
       opcode=Opcode, data_type=DataType, cas=Cas, key=Key,
       extras=Extras, body=Body, vbucket=VbucketId
      } = Request,
    
    KeyLen = byte_size(Key),
    ExtrasLen = byte_size(Extras),
    
    TotalBody = <<Extras/binary, Key/binary, Body/binary>>,
    TotalBodyLen = byte_size(TotalBody),
    
    <<16#80:8, Opcode:8, KeyLen:16, ExtrasLen:8, DataType:8, VbucketId:16,
      TotalBodyLen:32, 0:32, Cas:64, TotalBody/binary>>.

decode_response_header(<<16#81:8, Opcode:8, KeyLen:16, ExtrasLen:8, DataType:8,
                         Status:16, BodyLen:32, _Opaque:32, Cas:64>>) ->
    #rsp{
       opcode=Opcode,
       key_length=KeyLen,
       extras_length=ExtrasLen,
       data_type=DataType,
       status=get_status(Status),
       total_body_length=BodyLen,
       cas=Cas
      };
decode_response_header(Data) ->
    exit({invalid_response_header, Data}).

decode_response_body(Response, Bin) ->
    KeyLen = Response#rsp.key_length,
    ExtrasLen = Response#rsp.extras_length,
    <<Key:KeyLen/bytes, Extras:ExtrasLen/bytes, Body/binary>> = Bin,
    Response#rsp{key=Key, extras=Extras, body=Body}.

get_status(16#00) ->
    success;
get_status(16#01) ->
    key_enoent;
get_status(16#02) ->
    key_eexists;
get_status(16#03) ->
    e2big;
get_status(16#04) ->
    einval;
get_status(16#05) ->
    not_stored;
get_status(16#06) ->
    delta_badval;
get_status(16#07) ->
    not_my_vbucket;
get_status(16#20) ->
    auth_error;
get_status(16#21) ->
    auth_continue;
get_status(16#22) ->
    erange;
get_status(16#23) ->
    rollback;
get_status(16#81) ->
    unknown_command;
get_status(16#82) ->
    enomem;
get_status(16#83) ->
    not_supported;
get_status(16#84) ->
    einternal;
get_status(16#85) ->
    ebusy;
get_status(16#86) ->
    etmpfail;
get_status(_) ->
    unknown.

respond_with_cas(Response) ->
    case Response of
    	Rsp when Rsp#rsp.status =:= success ->
	    {ok, Rsp#rsp.cas};
	Rsp ->
	    {error, {Rsp#rsp.status, binary_to_list(Rsp#rsp.body)}}
    end.

respond_with_cas_and_body(Response) ->
    case Response of
    	Rsp when Rsp#rsp.status =:= success ->
	    {ok, Rsp#rsp.cas, Rsp#rsp.body};
	Rsp ->
	    {error, {Rsp#rsp.status, binary_to_list(Rsp#rsp.body)}}
    end.
