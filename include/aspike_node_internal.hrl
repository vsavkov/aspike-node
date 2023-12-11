-include("aspike.hrl").

-record(aspike_node_init_options, {
  node_id :: aspike:node_id(),
  endpoint :: #aspike_endpoint{},
  user :: #aspike_user{}
}).
