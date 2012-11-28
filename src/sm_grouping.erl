%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_grouping
-module(sm_grouping).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([shuffleGrouping/3]).

%%
%% API Functions
%%
shuffleGrouping(Topo, FromActorId, ToActorId) when is_record(Topo, topoConfig) ->
	IsActorIdValid = isActorIdValid({FromActorId, ToActorId}),	
	if 
		IsActorIdValid == false ->
			Topo;
		true->
			Conn = #connConfig{from = FromActorId, to = ToActorId, grouping = shuffleGrouping},
			OriginalConns = Topo#topoConfig.conns,
			_Topo1 = Topo#topoConfig{conns = [Conn|OriginalConns]}
	end.
	


%%
%% Local Functions
%%
isActorIdValid({FromActorId, ToActorId})->
	if
		FromActorId==[]; ToActorId==[]->
			false;
		true->
			true
	end.
	

