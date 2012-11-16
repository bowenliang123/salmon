%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine
-module(sardine).

%%
%% Include files
%%
-include("../include/sardine_topoconfig_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([cluster/2]).


-export([newTopo/0]).

-export([newSpout/2, newSpout/3]).
-export([setSpout/3, setSpout/4]).

-export([newBolt/2, newBolt/3]).
-export([setBolt/3, setBolt/4]).

-export([shuffleGrouping/3]).

-export([submitTopology/2]).


%%
%% API Functions
%%

%% Cluster
cluster(ZkIp, Port)->
	#clusterConfig{zkip = ZkIp, port = Port}.

%% Topology
newTopo()->
	sardine_topoBuilder:newTopo().
%% Spout
%% Add Spout
newSpout(Id, Module) ->
	sardine_spout:newSpout(Id, Module, 1).

newSpout(Id, Module, Parallelism_hint) ->
	sardine_spout:newSpout(Id, Module, Parallelism_hint).


setSpout(Topo, Id, Module) when is_record(Topo, topoConfig) ->
	sardine_topoBuilder:setSpout(Topo, Id, Module).

setSpout(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig)->
	sardine_topoBuilder:setSpout(Topo, Id, Module, Parallelism_hint).

%% Bolt
%% Add Bolt
newBolt(Id, Module) ->
	sardine_bolt:newBolt(Id, Module, 1).

newBolt(Id, Module, Parallelism_hint) when is_number(Parallelism_hint)->
	sardine_bolt:newBolt(Id, Module, Parallelism_hint).


setBolt(Topo, Id, Module) when is_record(Topo, topoConfig)->
	sardine_topoBuilder:setBolt(Topo, Id, Module).

setBolt(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig), is_number(Parallelism_hint)->
	sardine_topoBuilder:setBolt(Topo, Id, Module, Parallelism_hint).

%% Connection
%% Add Grouping
shuffleGrouping(Topo, FromActorId, ToActorId) when is_record(Topo, topoConfig) ->
	sardine_grouping:shuffleGrouping(Topo, FromActorId, ToActorId).

%% Submit
%% Check and Submit Topoloy Config to Zookeeper
submitTopology(Cluster, Topo) ->
	sardine_submitter:submitTopology(Cluster, Topo).

%%
%% Local Functions
%%

