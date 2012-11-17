%% Author: Administrator
%% Created: 2012-11-16
%% Description: TODO: Add description to sardine_submitter
-module(sardine_submitter).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

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
	VerifiedConnPairsList = verifyConns(ConnsConfigs, SpoutsConfigs, BoltsConfigs).


verifyConns(ConnsConfigs, SpoutsConfigs, BoltsConfigs) when is_list(ConnsConfigs) ->
	SpoutsIdList = getSpoutsIdList(SpoutsConfigs),
	BoltsIdList = getBoltsIdList(BoltsConfigs),
	verifyEachConnPair(ConnsConfigs, SpoutsIdList, BoltsIdList, []).



verifyEachConnPair([H|T] = ConnsConfigs, SpoutsIdList, BoltsIdList, ResultConnsConfigsList)
  when is_record(H, connConfig)->
	#connConfig{from = From, to = To} = H,
	if
		From==""; To==""->
			verifyEachConnPair(T, SpoutsIdList, BoltsIdList, ResultConnsConfigsList);
		true->
			Flag1 = sardine_common:isInList(From, lists:append([SpoutsIdList,BoltsIdList])),
			Flag2 = sardine_common:isInList(To, BoltsIdList),
			if
				Flag1 and Flag2 ->
					verifyEachConnPair(T, SpoutsIdList, BoltsIdList, [H|ResultConnsConfigsList]);
				true ->
					verifyEachConnPair(T, SpoutsIdList, BoltsIdList, ResultConnsConfigsList)
			end
   end;
verifyEachConnPair([], _, _, ResultConnsConfigsList)->
	ResultConnsConfigsList.

verifyConnPair({From, To}, SpoutsIdList, BoltsIdList) ->
	ok.
	

getSpoutsIdList(SpoutsConfigs) ->
	getSpoutsIdList(SpoutsConfigs,[]).

getSpoutsIdList([H|T] = SpoutsConfigs, ResultList) when is_record(H, spoutConfig) ->
	getSpoutsIdList(T, [H#spoutConfig.id|ResultList]);
getSpoutsIdList([],ResultList) ->
	ResultList.

getBoltsIdList(BoltsConfigs) ->
	getBoltsIdList(BoltsConfigs,[]).
getBoltsIdList([H|T] = BoltsConfigs, ResultList) when is_record(H, boltConfig) ->
	getBoltsIdList(T, [H#boltConfig.id|ResultList]);
getBoltsIdList([], ResultList) ->
	ResultList.


getConnPairsList(ConnsConfigs) ->
	genConnPairsList(ConnsConfigs,[]).
genConnPairsList([H|T] = ConnsConfigs, ResultList) when is_record(H, connConfig)->
	genConnPairsList(T, [{H#connConfig.from, H#connConfig.to}|ResultList]);
genConnPairsList([],ResultList)->
	ResultList.


