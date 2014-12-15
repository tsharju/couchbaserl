-record(request,
	{
	  magic = 16#80 :: integer(),
	  opcode = 0 :: integer(),
	  key_length = 0 :: integer(),
	  extras_length = 0 :: integer(),
	  data_type = 0 :: integer(),
	  total_body_length = 0 :: integer(),
	  cas = 0 :: integer(),
	  key = <<"">> :: binary(),
	  extras = <<"">> :: binary(),
	  body = <<"">> :: binary()
	}
       ).

-record(response,
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

-define(OP_SASL_LIST_MECHANISMS, 16#20).
-define(OP_SASL_AUTHENTICATE, 16#21).
-define(OP_SASL_STEP, 16#22).
