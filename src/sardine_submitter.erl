%% Author: Administrator
%% Created: 2012-11-16
%% Description: TODO: Add description to sardine_submitter
-module(sardine_submitter).

%%
%% Include files
%%
-include("../include/sardine_topoconfig_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([submitTopology/2]).

%%
%% API Functions
%%
submitTopology(Cluster, Topo) when is_record(Topo, topoConfig) ->
	Topo1 = parseConns(Topo).


%%
%% Local Functions
%%
parseConns(Topo) when is_record(Topo, topoConfig) ->
	#topoConfig{spouts = SpoutsConfigs, bolts = BoltsConfigs, conns = ConnsConfigs} = Topo,
	ConnsConfigs1 = valiadConns(ConnsConfigs).

valiadConns(ConnsConfigs) when is_list(ConnsConfigs) ->
	ok.