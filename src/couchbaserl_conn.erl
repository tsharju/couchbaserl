-module(couchbaserl_conn).
-behaviour(gen_server).

-include("couchbaserl.hrl").

%% API.
-export([start_link/0,
         authenticate/2,
         send_and_receive/2]).

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

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [{host, '127.0.0.1'}, {port, 11210}], []).

-spec authenticate(Username :: string(), Password :: string()) -> ok | {error, Reason :: string()}.
authenticate(Username, Password) ->
	gen_server:call(?MODULE, {authenticate, Username, Password}).

-spec send_and_receive(Socket :: gen_tcp:socket(), Request :: #request{}) -> #response{}.
send_and_receive(Socket, Request) ->
  send_request(Socket, Request),
  receive_response(Socket).

%% gen_server.
init([{host, Host}, {port, Port}]) ->
	Opts = [binary, {nodelay, true}, {active, false}, {packet, 0}],
	{ok, Socket} = gen_tcp:connect(Host, Port, Opts),
	{ok, #state{socket = Socket}}.

handle_call({authenticate, Username, Password}, _From, #state{socket=Socket} = State) ->
  Mechanisms = couchbaserl_auth:start_sasl(Socket),
  case lists:member('CRAM-MD5', Mechanisms) of
    true ->
      Result = couchbaserl_auth:sasl_cram_md5_auth(Socket, Username, Password),
      {reply, Result, State};
    false ->
      {reply, {error, not_implemented}, State}
  end;
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

send_request(Socket, Request) ->
  ReqData = encode_request(Request),
  case gen_tcp:send(Socket, ReqData) of
    ok ->
      ok;
    {error, Reason} ->
      exit(Reason)
  end.

receive_response(Socket) ->
  case gen_tcp:recv(Socket, ?HEADER_LENGTH) of
    {ok, Header} ->
      Response = decode_response_header(Header),
      {ok, Body} = gen_tcp:recv(Socket, Response#response.total_body_length),
      decode_response_body(Response, Body);
    {error, Reason} ->
      exit(Reason)
  end.

encode_request(Request) when is_record(Request, request) ->
  #request{
    opcode=Opcode, data_type=DataType, cas=Cas, key=Key,
    extras=Extras, body=Body
  } = Request,

  KeyLen = byte_size(Key),
  ExtrasLen = byte_size(Extras),

  TotalBody = <<Extras/binary, Key/binary, Body/binary>>,
  TotalBodyLen = byte_size(TotalBody),

  <<16#80:8, Opcode:8, KeyLen:16, ExtrasLen:8, DataType:8, 0:16,
    TotalBodyLen:32, 0:32, Cas:64, TotalBody/binary>>.

decode_response_header(<<16#81:8, Opcode:8, KeyLen:16, ExtrasLen:8, DataType:8,
                         Status:16, BodyLen:32, _Opaque:32, Cas:64>>) ->
	#response{
    opcode=Opcode,
    key_length=KeyLen,
    extras_length=ExtrasLen,
    data_type=DataType,
    status=Status,
    total_body_length=BodyLen,
    cas=Cas
	};
decode_response_header(Data) ->
  exit({invalid_response_header, Data}).

decode_response_body(Response, Bin) ->
  KeyLen = Response#response.key_length,
  ExtrasLen = Response#response.extras_length,
  <<Key:KeyLen, Extras:ExtrasLen, Body/binary>> = Bin,
  Response#response{key=Key, extras=Extras, body=Body}.
