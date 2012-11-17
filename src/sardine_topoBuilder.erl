%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_topoBuilder
-module(sardine_topoBuilder).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").



%%
%% Exported Functions
%%
-export([newTopo/0]).
-export([setSpout/2, setSpout/3, setSpout/4]).
-export([setBolt/2, setBolt/3, setBolt/4]).

%%
%% API Functions
%%
newTopo() ->
	#topoConfig{}.


%% adding Spout
setSpout(Topo, SpoutConfig) when is_record(Topo, topoConfig)->
	OriginalSpouts = Topo#topoConfig.spouts,
	Topo#topoConfig{spouts = [SpoutConfig|OriginalSpouts]}.
	

setSpout(Topo, Id, Module) when is_record(Topo, topoConfig) ->
	sardine:setSpout(Topo, Id, Module, 1).

setSpout(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig), is_number(Parallelism_hint) ->
	NewSpoutConfig = sardine_spout:newSpout(Id, Module, Parallelism_hint),
	Topo1 = sardine_topoBuilder:setSpout(Topo, NewSpoutConfig).

%% adding Bolt
setBolt(Topo, BoltConfig) when is_record(Topo, topoConfig) ->
	OriginalBolts = Topo#topoConfig.bolts,
	Topo#topoConfig{bolts = [BoltConfig|OriginalBolts]}.
	

setBolt(Topo, Id, Module) when is_record(Topo, topoConfig) ->
	setSpout(Topo, Id, Module, 1).

setBolt(Topo, Id, Module, Parallelism_hint) when is_record(Topo, topoConfig), is_number(Parallelism_hint) ->
	NewBoltConfig = sardine_bolt:newBolt(Id, Module, Parallelism_hint),
	Topo1 = sardine_topoBuilder:setBolt(Topo, NewBoltConfig).

%%
%% Local Functions
%%

