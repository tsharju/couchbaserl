-module(couchbaserl_conn).
-behaviour(gen_server).

-include("couchbaserl.hrl").

%% API.
-export([start_link/0,
         authenticate/2]).

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
	gen_server:start_link(?MODULE, [], []).

-spec authenticate(Username :: string(), Password :: string()) -> ok | {error, Reason :: string()}.
authenticate(Username, Password) ->
	gen_server:call(?MODULE, {authenticate, Username, Password}).

%% gen_server.
init([{host, Host}, {port, Port}]) ->
	Opts = [binary, {nodelay, true}, {header, ?HEADER_LENGTH}],
	{ok, Socket} = gen_tcp:connect(Host, Port, Opts),
	{ok, #state{socket = Socket}}.

handle_call({authenticate, Username, Password}, From, #state{socket=Socket} = State) ->
	send_auth_request(Username, Password),
	{reply, ok, State};
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

receive_response(Socket, Response) ->
	receive
		{tcp, Socket, Data} ->
			{H, [P]} = lists:split(?HEADER_LENGTH, Data),
			HeaderData = list_to_binary(H),
			case get_response_header(HeaderData) of
        {ok, Header} ->
          ok;
        {error, Reason} ->
          exit(Reason)
      end
  end.

get_response_header(<<Magic:8, Opcode:8, KeyLen:16, ExtrasLen:8, DataType:8, Status:16, BodyLen:32, _Opaque:32, Cas:64>>) ->
	{ok, #response_header{
    magic=Magic,
    opcode=Opcode,
    key_length=KeyLen,
    extras_length=ExtrasLen,
    data_type=DataType,
    status=Status,
    body_length=BodyLen,
    cas=Cas
	}};
get_response_header(Data) ->
  {error, {invalid_header, Data}}.
