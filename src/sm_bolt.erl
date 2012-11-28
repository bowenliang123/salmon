%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_bolt
-module(sm_bolt).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([newBolt/3,newBolt/4]).

%%
%% API Functions
%%
newBolt(TopoId, Id, Module) ->
	sm_bolt:newBolt(TopoId, Id, Module, 1).

newBolt(TopoId, Id, Module, Parallelism_hint) ->
	#boltConfig{id = Id, topoId=TopoId, module = Module, count = Parallelism_hint}.

%%
%% Local Functions
%%

