%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to sardine_grouping
-module(sardine_grouping).

%%
%% Include files
%%
-include("../include/sardine_topoconfig_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([shuffleGrouping/3]).

%%
%% API Functions
%%
shuffleGrouping(Topo, FromActorId, ToActorId) when is_record(Topo, topoConfig) ->
	Conn = #connConfig{from = FromActorId, to = ToActorId, grouping = shuffleGrouping},
	OriginalConns = Topo#topoConfig.conns,
	Topo1 = Topo#topoConfig{conns = [Conn|OriginalConns]}.


%%
%% Local Functions
%%

