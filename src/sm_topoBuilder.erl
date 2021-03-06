%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_topoBuilder
-module(sm_topoBuilder).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").



%%
%% Exported Functions
%%
-export([newTopo/0, newTopo/1]).
-export([setSpout/2, setSpout/4]).
-export([setBolt/2, setBolt/4]).

%%
%% API Functions
%%
newTopo() ->
	#topoConfig{}.
newTopo(TopologyId) ->
	#topoConfig{id = TopologyId}.


%% adding Spout
setSpout(Topo, SpoutConfig) when is_record(Topo, topoConfig)->
	OriginalSpouts = Topo#topoConfig.spouts,
	Topo#topoConfig{spouts = [SpoutConfig|OriginalSpouts]}.

setSpout(Topo, Id, Module, Parallelism_hint)
  when is_record(Topo, topoConfig) and is_number(Parallelism_hint) ->
	TopoId = Topo#topoConfig.id,
	SpoutConfig1 = sm_spout:newSpout(TopoId, Id, Module, Parallelism_hint),
	_Topo1 = sm_topoBuilder:setSpout(Topo, SpoutConfig1).

%% adding Bolt
setBolt(Topo, BoltConfig) when is_record(Topo, topoConfig) ->
	OriginalBolts = Topo#topoConfig.bolts,
	Topo#topoConfig{bolts = [BoltConfig|OriginalBolts]}.

setBolt(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig), is_number(Parallelism_hint) ->
	TopoId = Topo#topoConfig.id,
	BoltConfig1 = sm_bolt:newBolt(TopoId, Id, Module, Parallelism_hint),
	_Topo1 = sm_topoBuilder:setBolt(Topo, BoltConfig1).

%%
%% Local Functions
%%

