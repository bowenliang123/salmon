%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine
-module(sm).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([cluster/2]).


-export([newTopo/0, newTopo/1]).

-export([newSpout/2, newSpout/3]).
-export([setSpout/3, setSpout/4]).

-export([newBolt/2, newBolt/3]).
-export([setBolt/3, setBolt/4]).

-export([shuffleGrouping/3]).

-export([submitTopology/2]).

-export([publishTopology/2]).


%%
%% API Functions
%%

%% Cluster
cluster(ZkIp, Port)->
	#clusterConfig{zkip = ZkIp, port = Port}.

%% Topology
newTopo()->
	sm_topoBuilder:newTopo().
newTopo(TopologyId)->
	sm_topoBuilder:newTopo(TopologyId).
%% Spout
%% Add Spout
newSpout(Id, Module) ->
	sm_spout:newSpout(Id, Module, 1).

newSpout(Id, Module, Parallelism_hint) ->
	sm_spout:newSpout(Id, Module, Parallelism_hint).


setSpout(Topo, Id, Module) when is_record(Topo, topoConfig) ->
	sm_topoBuilder:setSpout(Topo, Id, Module).

setSpout(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig)->
	sm_topoBuilder:setSpout(Topo, Id, Module, Parallelism_hint).

%% Bolt
%% Add Bolt
newBolt(Id, Module) ->
	sm_bolt:newBolt(Id, Module, 1).

newBolt(Id, Module, Parallelism_hint) when is_number(Parallelism_hint)->
	sm_bolt:newBolt(Id, Module, Parallelism_hint).


setBolt(Topo, Id, Module) when is_record(Topo, topoConfig)->
	sm_topoBuilder:setBolt(Topo, Id, Module).

setBolt(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig), is_number(Parallelism_hint)->
	sm_topoBuilder:setBolt(Topo, Id, Module, Parallelism_hint).

%% Connection
%% Add Grouping
shuffleGrouping(Topo, FromActorId, ToActorId) when is_record(Topo, topoConfig) ->
	sm_grouping:shuffleGrouping(Topo, FromActorId, ToActorId).

%% Submit
%% Check and Submit Topoloy Config to Zookeeper
submitTopology(Cluster, Topo)
  when is_record(Cluster, clusterConfig) and is_record(Topo, topoConfig) ->
	Topo_1 = sm_submitter:submitTopology(Cluster, Topo),
	io:format("Topo to publish:~n~p~n",[Topo_1]),
	sm:publishTopology(Cluster, Topo_1).

publishTopology(Cluster, Topo)
  when is_record(Cluster, clusterConfig) and is_record(Topo, topoConfig) ->
	sm_publisher:publishTopology(Cluster, Topo).
%%
%% Local Functions
%%

