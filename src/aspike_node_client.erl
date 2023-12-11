-module(aspike_node_client).
-include("../include/aspike_node_internal.hrl").
-include("../include/aspike_protocol.hrl").
-include("../include/aspike_status.hrl").

%% API
-export([
]).

-dialyzer(no_undefined_callbacks).

-behavior(shackle_client).
-export([
  init/1,
  setup/2,
  handle_request/2,
  handle_data/2,
%%  handle_timeout/2, % optional callback
  terminate/1
]).

-record(auth, {
  status = not_authenticated, % not_authenticated, authenticated
  timestamp = 0, % erlang:system_time(second),
  ttl = 0, % seconds
  token = <<>> % binary
}).

-record(state, {
  init_options = #aspike_node_init_options{},
  auth = #auth{},
  buffer      = <<>> :: binary(),
  requests    = 0 :: non_neg_integer(),
  responses   = 0 :: non_neg_integer()
}).

%% public

-spec init(Options :: term()) ->
  {ok, State :: term()} |
  {error, Reason :: term()}.

init(Options) ->
  {ok, #state {init_options = Options}}.

setup(Socket, #state{init_options = #aspike_node_init_options{
      user = #aspike_user{name = Name, credential = Credential}}} = State) ->
  Auth = authenticate(Socket, Name, Credential),
  handle_authentication(Auth, State).

handle_request({put, {Namespace, Set, Key_digest, Bins}} = _Request, #state{requests = Counter} = State) ->
  Request_id = request_id(Counter),
  Pkt = aspike_protocol:enc_put_request(Namespace, Set, Key_digest, Bins),
  {ok, Request_id, Pkt, State#state {requests = Counter + 1}};

handle_request({get, {Namespace, Set, Key_digest}} = _Request, #state{requests = Counter} = State) ->
  Request_id = request_id(Counter),
  Pkt = aspike_protocol:enc_get_request(Namespace, Set, Key_digest, []),
  {ok, Request_id, Pkt, State#state {requests = Counter + 1}};

handle_request({get, {Namespace, Set, Key_digest, Bins}} = _Request, #state{requests = Counter} = State) ->
  Request_id = request_id(Counter),
  Pkt = aspike_protocol:enc_get_request(Namespace, Set, Key_digest, Bins),
  {ok, Request_id, Pkt, State#state {requests = Counter + 1}};

handle_request({remove, {Namespace, Set, Key_digest}} = _Request, #state{requests = Counter} = State) ->
  Request_id = request_id(Counter),
  Pkt = aspike_protocol:enc_remove_request(Namespace, Set, Key_digest),
  {ok, Request_id, Pkt, State#state {requests = Counter + 1}};

handle_request({exists, {Namespace, Set, Key_digest}} = _Request, #state{requests = Counter} = State) ->
  Request_id = request_id(Counter),
  Pkt = aspike_protocol:enc_exists_request(Namespace, Set, Key_digest),
  {ok, Request_id, Pkt, State#state {requests = Counter + 1}};

handle_request(_, _) ->
  % shackle_server will catch that
  {error, unexpected_request_to_handle}.

handle_data(Data, #state {buffer = Buffer, responses = Counter} = State) ->
  Data2 = <<Buffer/binary, Data/binary>>,
  {Decoded, Buffer2} = aspike_protocol:dec_responses(Data2),
  {Counter2, Responses} =
    lists:foldl(fun (X, {N, Ys}) -> {N+1, [{response_id(N), X}|Ys]} end,
    {Counter, []}, Decoded),
  {ok, lists:reverse(Responses), State#state {buffer = Buffer2, responses = Counter2}}.

%% Optional callback
%%handle_timeout(_, _) -> todo.

terminate(_) -> ok.

%% private
-define(MAX_REQUEST_ID, 4294967296).

request_id(Counter) ->
  Counter rem ?MAX_REQUEST_ID.

response_id(Counter) ->
  Counter rem ?MAX_REQUEST_ID.

authenticate(Socket, Name, Credential) ->
  Login = aspike_protocol:enc_login_request(Name, {?CREDENTIAL, Credential}),
  ok = gen_tcp:send(Socket, Login),
  Response = receive_response_login(Socket, 1000),
  aspike_protocol:dec_login_response(Response).

handle_authentication({ok, {?AEROSPIKE_OK, Fields}, Rest}, State) ->
  authenticated(Fields, Rest, State);
handle_authentication({ok, {?AEROSPIKE_SECURITY_NOT_SUPPORTED, Fields}, Rest}, State) ->
  authenticated(Fields, Rest, State);
handle_authentication({ok, {?AEROSPIKE_SECURITY_NOT_ENABLED, Fields}, Rest}, State) ->
  authenticated(Fields, Rest, State);
handle_authentication({ok, {Status, Fields}, _Rest}, State) ->
  Err = {aspike_status:status(Status), Fields},
  {error, Err, State};
handle_authentication({error, Reason}, State) ->
  {error, Reason, State}.

authenticated(Fields, Rest, State) ->
  Timestamp = erlang:system_time(second),
  Token = proplists:get_value(session_token, Fields),
  Ttl = proplists:get_value(session_ttl, Fields),
  {ok, State#state{buffer = Rest, auth = #auth{
    status = authenticated,
    token = Token, ttl = Ttl, timestamp = Timestamp}}}.

receive_response_login(Socket, Timeout) ->
  receive_proto(Socket, 8, Timeout, ?AS_ADMIN_MESSAGE_TYPE).

receive_proto(Socket, Header_size, Timeout, Type) ->
  Ret_header = gen_tcp:recv(Socket, Header_size, Timeout),
  case Ret_header of
    {ok, Header} ->
      <<?AS_PROTO_VERSION:8, Type:8, Size:48/big-unsigned-integer>>
        = Header,
      Ret_data = gen_tcp:recv(Socket, Size, Timeout),
      case Ret_data of
        {ok, Data} -> <<Header/binary, Data/binary>>;
        Other_data -> Other_data
      end;
    Other_header -> Other_header
  end.
