%% Author: Administrator
%% Created: 2012-11-21
%% Description: TODO: Add description to sardine_publisher
-module(sardine_publisher).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").
%%
%% Exported Functions
%%
-export([publishTopology/2]).

%%
%% API Functions
%%
publishTopology(Cluster, Topo)
	when is_record(Cluster, clusterConfig) and is_record(Topo, topoConfig) ->
		Topo,
		#clusterConfig{zkip = ZkIp, port = Port} = Cluster,
		ZkServer = {ZkIp, Port, 30000, 100000},
		{ok, ConnPid} = sardine_zk:startConnection([ZkServer]).
		

%%
%% Local Functions
%%

