-module(aspike_node_response).
-include("../include/aspike_protocol.hrl").
-include("../include/aspike_status.hrl").

%% API
-export([
  handle/2
  ]).

-spec handle(aspike:op(), aspike:response()) -> aspike:handled_response().
handle(Op, {ok, Response}) ->
  format(Op, Response);
handle(Op, {error, Reason}) ->
  {error, {Op, Reason}};
handle(Op, X) ->
  {error, {unrecognized_op_response, Op, X}}.

format(put,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_OK}, _, _}) ->
  ok;
format(put,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = Status}, _, _}) ->
  {error, aspike_status:status(Status)};

format(get,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_OK}, _Fields, Ops}) ->
  {ok, [{binary_to_list(Bin), V} || {Bin, V} <- Ops]};
format(get,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_ERR_RECORD_NOT_FOUND}, _, _}) ->
  {error, record_not_found};
format(get,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = Status}, _, _}) ->
  {error, aspike_status:status(Status)};

format(remove,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_OK}, _, _}) ->
  ok;
format(remove,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_ERR_RECORD_NOT_FOUND}, _, _}) ->
  {error, record_not_found};
format(remove,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = Status}, _, _}) ->
  {error, aspike_status:status(Status)};

format(exists,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_OK}, _, _}) ->
  true;
format(exists,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = ?AEROSPIKE_ERR_RECORD_NOT_FOUND}, _, _}) ->
  false;
format(exists,
    {#as_proto{version = ?AS_PROTO_VERSION, type = ?AS_MESSAGE_TYPE},
      #as_msg{result_code = Status}, _, _}) ->
  {error, aspike_status:status(Status)};

format(Op, Response) ->
  {error, {unrecognized_op_response, Op, Response}}.
