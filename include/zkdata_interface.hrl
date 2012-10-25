%% 
-record(worker_info, {
self_name = null_server,
node_name = null_node}).

-record(type_info, {
module = null,
count = 0}).


-record(server_state, {
self_name = null_selfname,
topo_id = null_id,
type = null_type,
type_name = null,
index = -1,
module = null_module,
to_server = null_server,
to_node = null_node}).

%% 

