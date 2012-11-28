%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_spout
-module(sm_spout).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([newSpout/3,newSpout/4]).

%%
%% API Functions
%%
newSpout(TopoId, Id, Module)->
	sm_spout:newSpout(TopoId, Id, Module, 1).

newSpout(TopoId, Id, Module, Parallelism_hint) ->
	#spoutConfig{id = Id, topoId=TopoId, module = Module, count = Parallelism_hint}.


%%
%% Local Functions
%%

