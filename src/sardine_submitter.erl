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
%% 	ConnsConfigs1 = valiadConns(ConnsConfigs).
	getBoltsIdList(BoltsConfigs).

valiadConns(ConnsConfigs) when is_list(ConnsConfigs) ->
	ConnPairsList = getConnPairsList(ConnsConfigs).

getSpoutsIdList(SpoutsConfigs) ->
	getSpoutsIdList(SpoutsConfigs,[]).
getSpoutsIdList([],ResultList) ->
	ResultList;
getSpoutsIdList([H|T] = SpoutsConfigs, ResultList) when is_record(H, spoutConfig) ->
	getSpoutsIdList(T, [{{H#spoutConfig.id}}|ResultList]).

getBoltsIdList(BoltsConfigs) ->
	getBoltsIdList(BoltsConfigs,[]).
getBoltsIdList([], ResultList) ->
	ResultList;
getBoltsIdList([H|T] = BoltsConfigs, ResultList) when is_record(H, boltConfig) ->
	getBoltsIdList(T, [{H#boltConfig.id}|ResultList]).


getConnPairsList(ConnsConfigs) ->
	genConnPairsList(ConnsConfigs,[]).
genConnPairsList([],ResultList)->
	ResultList;
genConnPairsList([H|T] = ConnsConfigs, ResultList) when is_record(H, connConfig)->
	#connConfig{from = From, to = To} = H,
	genConnPairsList(T, [{From, To}|ResultList]).


