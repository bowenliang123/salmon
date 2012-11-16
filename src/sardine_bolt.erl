%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_bolt
-module(sardine_bolt).

%%
%% Include files
%%
-include("../include/sardine_topoconfig_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([newBolt/2,newBolt/3]).

%%
%% API Functions
%%
newBolt(Id, Module) ->
	sardine_bolt:newBolt(Id, Module,1).
newBolt(Id, Module, Parallelism_hint) ->
	#boltConfig{id = Id, module = Module, count = Parallelism_hint}.

%%
%% Local Functions
%%

