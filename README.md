aspike_node
=====

Erlang Aerospike Client

#### Features

* Uses Aerospike Binary protocol, ```aspike-protocol```, https://github.com/vsavkov/aspike-protocol
* Uses ```shackle```, High-Performance Erlang Network Client Framework, https://github.com/lpgauth/shackle

### Build
```bash
$ rebar3 compile
```


### Examples

#### Environments to run examples
- Aerospike Cluster Standard/Enterprise/Cloud (https://aerospike.com/products/features-and-editions/);
- Aerospike Cluster Community Edition (CE) (https://hub.docker.com/r/aerospike/aerospike-server);
- Aerospike Server Emulator (EM) (https://github.com/vsavkov/aspike-server).

##### Aerospike Cluster Community Edition (CE) Setup
Follow the instructions on https://hub.docker.com/r/aerospike/aerospike-server

##### Aerospike Server Emulator (EM) Setup
1. Clone https://github.com/vsavkov/aspike-server;
2. From terminal, in `aspike-server` directory, run
```bash
iex -S mix
[info] Accepting connections on port 4040
[info] Accepting connections on port 4041
```
Aerospike Server Emulator accepts its own `text protocol` on port `4040`.

Aerospike Server Emulator accepts Aerospike `binary protocol` on port `4041`.

3. Create `namespace` `test` for the following examples

- From another terminal run `telnet` or `nc` (aka `netcat`)
```bash
nc -vv 127.0.0.1 4040
```

- To create `namespace` `test`, type in
```
CREATE test
```

- To check that `namespace` `test` exists, type in
```
NAMESPACES
```

```
[test]
OK
indicates that `namespace` `test` exists.
```

## aspike-node Examples

From `aspike-node` directory run
```
$ rebar3 shell
```
Start `shackle`
```erlang
1> application:ensure_all_started(shackle).
{ok,[granderl,metal,compiler,syntax_tools,foil,shackle]}
```
Prepare parameters to connect to an Aerospike node
```erlang
2> rr("include/aspike.hrl").
[aspike_connection_options,aspike_endpoint,aspike_node_params,aspike_pool_options,aspike_user]
```
### Aerospike Server, Community Edition
```erlang
3> Endpoint_CE = #aspike_endpoint{name = "Node_CE", address = "127.0.0.1", port = 3000}.
4> User_CE = #aspike_user{name = "User", credential = "password"}.
5> Socket_options = [{mode, binary}, {packet, raw}, {buffer, 65535}, {nodelay, true}, {send_timeout, 50}, {send_timeout_close, true}].
6> Connection_options = #aspike_connection_options{reconnect = false, reconnect_time_min = 2_000, reconnect_time_max = 120_000, socket_options = Socket_options}.
7> Pool_options = #aspike_pool_options{backlog_size = 1_024, max_retries = 0, pool_size = 1, pool_strategy = random}.
8> Node_params_CE = #aspike_node_params{endpoint = Endpoint_CE, user = User_CE, connection_options = Connection_options, pool_options = Pool_options}.
9> Node_id_CE = node_CE.
11> #aspike_pool_options{pool_size = Pool_size} = Pool_options.
12> Pool_size.
1
```
Connect to the node
```erlang
10> aspike_node:start(Node_id_CE, Node_params_CE).
ok
```
Check that connection(s) established 
```erlang
11> [shackle_status:active({Node_id_CE, I}) || I <- lists:seq(1, Pool_size)].
[true]
```
Prepare key digest
```erlang
12> Set = "set1", Key1 = "key1", Key1_digest = aspike_protocol:digest(Set, Key1).
```
Put key-value
```erlang
13> aspike_node:put(Node_id_CE, "test", Set, Key1_digest, [{"bin1", "value1"}]).
ok
```
Get key-value
```erlang
14> aspike_node:get(Node_id_CE, "test", Set, Key1_digest, []).
{ok,[{"bin1","value1"}]}
```
Check, if key exists
```erlang
15> aspike_node:exists(Node_id_CE, "test", Set, Key1_digest).
true
```
Remove key
```erlang
16> aspike_node:remove(Node_id_CE, "test", Set, Key1_digest).
ok
```
Check, if the removed key exists
```erlang
17> aspike_node:exists(Node_id_CE, "test", Set, Key1_digest).
false
```
Get the removed key
```erlang
18> aspike_node:get(Node_id_CE, "test", Set, Key1_digest, []).
{error,record_not_found}
```
Async put key-value
```erlang
19> Ref_put = aspike_node:async_put(Node_id_CE, "test", Set, Key1_digest, [{"bin1", "value1"}]).
20> aspike_node:receive_response(Ref_put).
ok
```
Async get key
```erlang
21> Ref_get = aspike_node:async_get(Node_id_CE, "test", Set, Key1_digest, []).
22> aspike_node:receive_response(Ref_get).
{ok,[{"bin1","value1"}]}
```

### Aerospike Server Emulator
```erlang
3> Endpoint_EM = #aspike_endpoint{name = "Node_EM", address = "127.0.0.1", port = 4041}.
```
Aerospike Server Emulator, as Aerospike Cluster Standard/Enterprise/Cloud, requires Blowfish-encrypted password.

(WARNING! The encryption rate of the implementation is slow, could take 2-5 seconds to encrypt password)
```erlang
4> Credential = aspike_blowfish:crypt("pass1").
<<"$2a$10$7EqJtq98hPqEX7fNZaFWoOOY1Ba9.gZNwHJkrSKJl7mXQyPCsCrQa">>
```
```erlang
4> User_EM = #aspike_user{name = "User1", credential = Credential}.
5> Socket_options = [{mode, binary}, {packet, raw}, {buffer, 65535}, {nodelay, true}, {send_timeout, 50}, {send_timeout_close, true}].
6> Connection_options = #aspike_connection_options{reconnect = false, reconnect_time_min = 2_000, reconnect_time_max = 120_000, socket_options = Socket_options}.
7> Pool_options_EM = #aspike_pool_options{backlog_size = 1_024, max_retries = 0, pool_size = 3, pool_strategy = random}.
8> Node_params_EM = #aspike_node_params{endpoint = Endpoint_EM, user = User_EM, connection_options = Connection_options, pool_options = Pool_options_EM}.
9> Node_id_EM = node_EM.
11> #aspike_pool_options{pool_size = Pool_size_EM} = Pool_options_EM.
12> Pool_size_EM.
3
```
Connect to the node
```erlang
10> aspike_node:start(Node_id_EM, Node_params_EM).
ok
```
Check that connection(s) established
```erlang
11> [shackle_status:active({Node_id_EM, I}) || I <- lists:seq(1, Pool_size_EM)].
[true,true,true]
```
Prepare key digest
```erlang
12> Set_EM = "set1", Key_EM = "key1-EM", Key_EM_digest = aspike_protocol:digest(Set_EM, Key_EM).
```
Put key-value
```erlang
13> aspike_node:put(Node_id_EM, "test", Set_EM, Key_EM_digest, [{"bin1_EM", "value1_EM"}]).
```
If `namespace` `test` was not created as descibed in _Aerospike Server Emulator (EM) Setup_, then the response will be as follows
```erlang
{error,{20,<<"AEROSPIKE_ERR_NAMESPACE_NOT_FOUND">>, <<"Namespace in request not found on server.">>}}
```
Otherwise,
```erlang
ok
```
Get key-value
```erlang
14> aspike_node:get(Node_id_EM, "test", Set_EM, Key_EM_digest, []).
{ok,[{"bin1_EM","value1_EM"}]}
```
Check, if key exists
```erlang
15> aspike_node:exists(Node_id_EM, "test", Set_EM, Key_EM_digest).
true
```
Remove key
```erlang
16> aspike_node:remove(Node_id_EM, "test", Set_EM, Key_EM_digest).
ok
```
Check, if the removed key exists
```erlang
17> aspike_node:exists(Node_id_EM, "test", Set_EM, Key_EM_digest).
false
```
Get the removed key
```erlang
18> aspike_node:get(Node_id_EM, "test", Set_EM, Key_EM_digest, []).
{error,record_not_found}
```
Async put key-value
```erlang
19> Ref_put_EM = aspike_node:async_put(Node_id_EM, "test", Set_EM, Key_EM_digest, [{"bin2_EM", "value2_EM"}, {"bin3_EM", 123}]).
20> aspike_node:receive_response(Ref_put_EM).
ok
```
Async get key
```erlang
21> Ref_get_EM = aspike_node:async_get(Node_id_EM, "test", Set_EM, Key_EM_digest, []).
22> aspike_node:receive_response(Ref_get_EM).
{ok,[{"bin2_EM","value2_EM"},{"bin3_EM",123}]}
```
