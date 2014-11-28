-record(request_header, {
  magic :: integer(),
  opcode :: integer(),
  key_length :: integer(),
  extras_length :: integer(),
  data_type :: integer(),
  body_length :: integer(),
  cas :: integer()
}).

-record(response_header, {
  magic :: integer(),
  opcode :: integer(),
  key_length :: integer(),
  extras_length :: integer(),
  data_type :: integer(),
  status :: integer(),
  body_length :: integer(),
  cas :: integer()
}).
