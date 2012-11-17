%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_spout
-module(sardine_spout).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([newSpout/2,newSpout/3]).

%%
%% API Functions
%%
newSpout(Id, Module)->
	sardine_spout:newSpout(Id, Module,1).

newSpout(Id, Module, Parallelism_hint) ->
	#spoutConfig{id = Id, module = Module, count = Parallelism_hint}.


%%
%% Local Functions
%%

