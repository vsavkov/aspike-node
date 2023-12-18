-module(aspike_node).
-include("../include/aspike_node_internal.hrl").

-dialyzer({nowarn_function, [
  start/2, stop/1,
  receive_response/1,
  call/2, cast/2]}).

%% API
-export([
  start/2,
  stop/1,
  put/5,
  async_put/5,
  get/4,
  get/5,
  async_get/4,
  async_get/5,
  remove/4,
  async_remove/4,
  exists/4,
  async_exists/4,
  receive_response/1
]).

%% public
-spec start(aspike:node_id(), aspike:node_params()) ->
  ok | {error, shackle_not_started |
                aspike_node_already_started}.
start(Node_id, #aspike_node_params{
  endpoint = #aspike_endpoint{
    name = _Name, address = Address, port = Port
  } = Endpoint,
  user = User,
  connection_options = #aspike_connection_options{
    reconnect = Reconnect,
    reconnect_time_max = Reconnect_time_max,
    reconnect_time_min = Reconnect_time_min,
    socket_options = Socket_options
  },
  pool_options = #aspike_pool_options{
    backlog_size = Backlog_size,
    max_retries = Max_retries,
    pool_size = Pool_size,
    pool_strategy = Pool_strategy
  }
}) ->

  Address_str = if is_binary(Address) -> binary_to_list(Address); true -> Address end,
  Endpoint1 = Endpoint#aspike_endpoint{address = Address_str},

  Init_options = #aspike_node_init_options{
    node_id = Node_id,
    endpoint = Endpoint1,
    user = User},
  ClientOptions = [
    {init_options, Init_options},
    {address, Address_str},
    {port, Port},
    {protocol, shackle_tcp},
    {reconnect, Reconnect},
    {reconnect_time_max, Reconnect_time_max},
    {reconnect_time_min, Reconnect_time_min},
    {socket_options, Socket_options}
  ],
  PoolOptions = [
    {backlog_size, Backlog_size},
    {max_retries, Max_retries},
    {pool_size, Pool_size},
    {pool_strategy, Pool_strategy}
  ],

  case shackle_pool:start(Node_id, aspike_node_client,
    ClientOptions, PoolOptions) of
    ok -> ok;
    {error, shackle_not_started} = Err -> Err;
    {error, pool_already_started} -> {error, aspike_node_already_started}
  end.

-spec stop(aspike:node_id()) -> ok | {error, shackle_not_started | aspike_node_not_started}.
stop(Node_id) ->
  case shackle_pool:stop(Node_id) of
    ok -> ok;
    {error, shackle_not_started} = Err -> Err;
    {error, pool_not_started} -> {error, aspike_node_not_started}
  end.

-spec put(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest(), aspike:bins()) ->
  ok | {error, aspike:status()}.
put(Node_id, Namespace, Set, Key_digest, Bins) ->
  call(Node_id, {put, {Namespace, Set, Key_digest, Bins}}).

-spec async_put(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest(), aspike:bins()) -> {put, aspike:response_id()} | {error, atom()}.
async_put(Node_id, Namespace, Set, Key_digest, Bins) ->
  cast(Node_id, {put, {Namespace, Set, Key_digest, Bins}}).

-spec get(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest()) ->
  {ok, aspike:bins()} | {error, record_not_found | aspike:status()}.
get(Node_id, Namespace, Set, Key_digest) ->
  call(Node_id, {get, {Namespace, Set, Key_digest}}).

-spec get(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest(), aspike:bins()) ->
  {ok, aspike:bins()} | {error, record_not_found | aspike:status()}.
get(Node_id, Namespace, Set, Key_digest, Bins) ->
  call(Node_id, {get, {Namespace, Set, Key_digest, Bins}}).

-spec async_get(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest()) -> {get, aspike:response_id()} | {error, atom()}.
async_get(Node_id, Namespace, Set, Key_digest) ->
  cast(Node_id, {get, {Namespace, Set, Key_digest}}).

-spec async_get(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest(), aspike:bins()) -> {get, aspike:response_id()} | {error, atom()}.
async_get(Node_id, Namespace, Set, Key_digest, Bins) ->
  cast(Node_id, {get, {Namespace, Set, Key_digest, Bins}}).

-spec remove(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest()) ->
  {ok, aspike:bins()} | {error, record_not_found | aspike:status()}.
remove(Node_id, Namespace, Set, Key_digest) ->
  call(Node_id, {remove, {Namespace, Set, Key_digest}}).

-spec async_remove(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest()) -> {remove, aspike:response_id()} | {error, atom()}.
async_remove(Node_id, Namespace, Set, Key_digest) ->
  cast(Node_id, {remove, {Namespace, Set, Key_digest}}).

-spec exists(aspike:node_id(), aspike:namespace(), aspike:set(), aspike:key_digest()) ->
%%  {true | false | aspike:status()}.
  aspike:handled_response().
exists(Node_id, Namespace, Set, Key_digest) ->
  call(Node_id, {exists, {Namespace, Set, Key_digest}}).

-spec async_exists(aspike:node_id(), aspike:namespace(), aspike:set(),
    aspike:key_digest()) -> {exists, aspike:response_id()} | {error, atom()}.
async_exists(Node_id, Namespace, Set, Key_digest) ->
  cast(Node_id, {exists, {Namespace, Set, Key_digest}}).

-spec receive_response({aspike:op(), aspike:response_id()}) -> aspike:handled_response().
receive_response({Op, RequestId}) ->
  aspike_node_response:handle(Op, shackle:receive_response(RequestId)).

%% private
call(Node_id, {Op, _Data} = Request) ->
  aspike_node_response:handle(Op, shackle:call(Node_id, Request)).

cast(Node_id, {Op, _Data} = Request) ->
  case shackle:cast(Node_id, Request) of
    {ok, RequestId} -> {Op, RequestId};
    Error -> Error
  end.
