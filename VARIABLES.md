# Configuration of the Node

The node can be configured via environment variables or command-line arguments. 
Below is a summary of the environment variables available. To see a full list of available variables provide the
`--help` flag to the executable. 

## Common
Common configurations for the node. These options are shared among the different modes of operations for nodes. 

- `CONCORDIUM_NODE_ID` Force sets the node id. 
Note the id must be a 64 bit unsigned integer in zero padded HEX. Must be 16 characters long.

- `CONCORDIUM_NODE_LISTEN_ADDRESS` The address on which the node listens on. 

- `CONCORDIUM_NODE_LISTEN_PORT` The port on which the node is listening for incoming connections. 
The port should be reachable, hence the port should be open in any firewall rules. The default value is 8888.

- `CONCORDIUM_NODE_EXTERNAL_PORT` is related to the `CONCORDIUM_NODE_LISTEN_PORT`. If the external port of the ***server*** is not the same as the listening port i.e., it has been remapped. 
Then this should be set to the external port in order to allow other nodes to connect to this node.

- `CONCORDIUM_NODE_CONFIG_DIR` Where the node should store its configuration.

- `CONCORDIUM_NODE_DATA_DIR` Where the node should store its data, in particular the nodes database is stored here.

## Baker
Configurations related to baking.

- `CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE` A path to the file containing the baker keys. The filepath must be either an absolute path or a relative filepath to the CWD of the process. If this variable is not set, then the node is not eligible for baking. 

## Connection
Network related configurations for a node.

- `CONCORDIUM_NODE_CONNECTION_DESIRED_NODES` Specifies the minimum number of nodes that the node wishes. 
If the node has fewer connected nodes than specified, then the node will try to acquire more nodes via bootstrapping or requesting more nodes from its existing peers. The default value is 7.

- `CONCORDIUM_NODE_CONNECTION_MAX_ALLOWED_NODES` Is the maximum allowed number of peers the node will tolerate. 
If the node gets more than specified, then the node will start dropping its peers until the number of peers is below the specified amount.

- `CONCORDIUM_NODE_CONNECTION_MAX_ALLOWED_NODES_PERCENTAGE` Is the default way of setting the maximum number of peers, that the node will tolerate. 
This variable is set as a percentage wrt. `CONCORDIUM_NODE_CONNECTION_DESIRED_NODES`. The default value is 150.

- `CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES` A comma separated list of URLs specifying the first nodes that the node should connect to. (This option disables DNS bootstrapping feature).

- `CONCORDIUM_NODE_CONNECTION_HARD_CONNECTION_LIMIT` Is the maximum number of ***connections*** (as opposed to nodes) that a node will have at a given time. 
This should be set a bit higher than the maximum number of nodes, so that new peers are accepted and discovered over time. The default value is 20. 

- `CONCORDIUM_NODE_CONNECTION_THREAD_POOL_SIZE` Specifies the thread pool size of the node for handling connection events in parallel. The default value is 4. 

## gRPC
Configuration parameters related to the built-in gRPC server.

- `CONCORDIUM_NODE_DISABLE_RPC_SERVER` Disables the gRPC server. Default the RPC server is turned on.

- `CONCORDIUM_NODE_RPC_SERVER_ADDR` Is the listen address of the node's gRPC server. The default value is 127.0.0.1.

- `CONCORDIUM_NODE_RPC_SERVER_PORT` Is the listen port of the node's gRPC server. 
The default value is 10000. (Note if `CONCORDIUM_NODE_RPC_SERVER_ADDR` or `CONCORDIUM_NODE_RPC_SERVER_PORT` are changed, then the variable `CONCORDIUM_NODE_COLLECTOR_GRPC_HOST` must be changed accordingly for the node-collector-service)
