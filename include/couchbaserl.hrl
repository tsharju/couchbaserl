-record(req,
	{
	  magic = 16#80 :: integer(),
	  opcode = 0 :: integer(),
	  key_length = 0 :: integer(),
	  extras_length = 0 :: integer(),
	  data_type = 0 :: integer(),
	  vbucket = 0 :: integer(),
	  total_body_length = 0 :: integer(),
	  cas = 0 :: integer(),
	  key = <<"">> :: binary(),
	  extras = <<"">> :: binary(),
	  body = <<"">> :: binary()
	}
       ).

-record(rsp,
	{
	  magic = 16#81 :: integer(),
	  opcode = 0 :: integer(),
	  key_length = 0 :: integer(),
	  extras_length = 0 :: integer(),
	  data_type = 0 :: integer(),
	  status = 0 :: integer(),
	  total_body_length = 0 :: integer(),
	  cas = 0 :: integer(),
	  key = <<"">> :: binary(),
	  extras = <<"">> :: binary(),
	  body = <<"">> :: binary()
	}
       ).

-define(OP_GET, 16#00).
-define(OP_SET, 16#01).
-define(OP_ADD, 16#02).
-define(OP_REPLACE, 16#03).
-define(OP_DELETE, 16#04).
-define(OP_INCREMENT ,16#05).
-define(OP_DECREMENT ,16#06).
-define(OP_QUIT, 16#07).
-define(OP_FLUSH, 16#08).
-define(OP_SASL_LIST_MECHANISMS, 16#20).
-define(OP_SASL_AUTHENTICATE, 16#21).
-define(OP_SASL_STEP, 16#22).
-define(OP_GET_CLUSTER_CONFIG, 16#b5).
