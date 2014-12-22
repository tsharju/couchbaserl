-module(couchbaserl_auth).

-export([sasl_cram_md5/3, decode_mechanisms/1]).

-include("couchbaserl.hrl").

-spec sasl_cram_md5(Challenge :: binary(), BucketName :: string(),
		    Password :: string()) -> ok | {error, any()}.
sasl_cram_md5(Challenge, BucketName, Password) ->
    Digest = crypto:hmac(md5, Password, Challenge),
    HexDigest = hex(Digest),

    list_to_binary(BucketName ++ " " ++ HexDigest).

-spec decode_mechanisms(Data :: binary()) -> [atom()].
decode_mechanisms(Data) ->
    S = binary_to_list(Data),
    lists:map(
      fun(M) ->
	      list_to_atom(M)
      end,
      string:tokens(string:to_upper(S), " ")
     ).

%% Private API

hex(Data) ->
    lists:flatten(
      [io_lib:format("~2.16.0b",[H]) || H <- binary_to_list(Data)]).
