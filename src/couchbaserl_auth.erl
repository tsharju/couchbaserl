-module(couchbaserl_auth).

-export([start_sasl/1, sasl_cram_md5_auth/3]).

-include("couchbaserl.hrl").

-spec start_sasl(Socket :: gen_tcp:socket()) -> [atom()].
start_sasl(Socket) ->
  Request = #request{opcode=?OP_SASL_LIST_MECHANISMS},
  Response = couchbaserl_conn:send_and_receive(Socket, Request),
  decode_mechanisms(Response#response.body).


-spec sasl_cram_md5_auth(Socket :: gen_tcp:socket(), Username :: string(), Password :: string) -> ok | {error, atom()}.
sasl_cram_md5_auth(Socket, Username, Password) ->
  Request = #request{opcode = ?OP_SASL_AUTHENTICATE, key = <<"CRAM-MD5">>},
  Response = couchbaserl_conn:send_and_receive(Socket, Request),

  Challenge = Response#response.body,
  Digest = crypto:hmac(md5, Password, Challenge),
  HexDigest = hex(Digest),

  Result = list_to_binary(Username ++ " " ++ HexDigest),
  AuthRequest = #request{opcode = ?OP_SASL_STEP, key = <<"CRAM-MD5">>, body = Result},
  AuthResponse = couchbaserl_conn:send_and_receive(Socket, AuthRequest),

  case AuthResponse#response.body of
    <<"Authenticated">> ->
      ok;
    <<"Auth failure">> ->
      {error, auth_failure};
    _ ->
      {error, auth_error}
  end.

%% Private API

decode_mechanisms(Data) ->
  S = binary_to_list(Data),
  lists:map(
    fun(M) ->
      list_to_atom(M)
    end,
    string:tokens(string:to_upper(S), " ")
  ).

hex(Data) ->
  lists:flatten(
    [io_lib:format("~2.16.0b",[H]) || H <- binary_to_list(Data)]).
