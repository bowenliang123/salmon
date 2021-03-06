%% Author: Administrator
%% Created: 2012-11-16
%% Description: TODO: Add description to sardine_submitter
-module(sm_submitter).

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
submitTopology(Cluster, TopoConfig) when is_record(TopoConfig, topoConfig) ->
	TopoConfig_1 = parseConns(TopoConfig),
	TopoConfig_2 = varifyActorFilesReady(TopoConfig_1).
	


%%
%% Local Functions
%%
parseConns(Topo) when is_record(Topo, topoConfig) ->
	#topoConfig{spouts = SpoutsConfigs, bolts = BoltsConfigs, conns = ConnsConfigs} = Topo,
	VerifiedConnPairsList = verifyConns(ConnsConfigs, SpoutsConfigs, BoltsConfigs),
	{SpoutsConfigs1,BoltsConfigs1} = putConnPairs(VerifiedConnPairsList,SpoutsConfigs,BoltsConfigs),
	_Topo1 = Topo#topoConfig{spouts = SpoutsConfigs1, bolts = BoltsConfigs1}.


verifyConns(ConnsConfigs, SpoutsConfigs, BoltsConfigs) when is_list(ConnsConfigs) ->
	SpoutsIdList = getSpoutsIdList(SpoutsConfigs),
	BoltsIdList = getBoltsIdList(BoltsConfigs),
	verifyEachConnPair(ConnsConfigs, SpoutsIdList, BoltsIdList, []).



verifyEachConnPair([H|T] = _ConnsConfigs, SpoutsIdList, BoltsIdList, ResultConnsConfigsList)
  when is_record(H, connConfig)->
	#connConfig{from = From, to = To} = H,
	if
		From==""; To==""->
			verifyEachConnPair(T, SpoutsIdList, BoltsIdList, ResultConnsConfigsList);
		true->
			Flag1 = sm_utils:isInList(From, lists:append([SpoutsIdList,BoltsIdList])),
			Flag2 = sm_utils:isInList(To, BoltsIdList),
			if
				Flag1 and Flag2 ->
					verifyEachConnPair(T, SpoutsIdList, BoltsIdList, [H|ResultConnsConfigsList]);
				true ->
					verifyEachConnPair(T, SpoutsIdList, BoltsIdList, ResultConnsConfigsList)
			end
   end;
verifyEachConnPair([], _, _, ResultConnsConfigsList)->
	ResultConnsConfigsList.
	

getSpoutsIdList(SpoutsConfigs) ->
	getSpoutsIdList(SpoutsConfigs,[]).

getSpoutsIdList([H|T] = _SpoutsConfigs, ResultList) when is_record(H, spoutConfig) ->
	getSpoutsIdList(T, [H#spoutConfig.id|ResultList]);
getSpoutsIdList([], ResultList) ->
	ResultList.

getBoltsIdList(BoltsConfigs) ->
	getBoltsIdList(BoltsConfigs,[]).
getBoltsIdList([H|T] = _BoltsConfigs, ResultList) when is_record(H, boltConfig) ->
	getBoltsIdList(T, [H#boltConfig.id|ResultList]);
getBoltsIdList([], ResultList) ->
	ResultList.

%% 将Conn连接信息写入SpoutsConfigs、BoltConfigs
putConnPairs(VerifiedConnPairsList, SpoutsConfigs, BoltsConfigs) ->
	SpoutsConfigs1 = putConnPairsToSpouts(VerifiedConnPairsList, SpoutsConfigs),
	BoltsConfigs1 = putConnPairsToBolts(VerifiedConnPairsList, BoltsConfigs),
	{SpoutsConfigs1, BoltsConfigs1}.

%% 将Conn连接信息写入Spouts的From和To字段
putConnPairsToSpouts(VerifiedConnCongfigsList, SpoutsConfigs) ->
	putConnCongfigsToSpouts(VerifiedConnCongfigsList, SpoutsConfigs, []).

putConnCongfigsToSpouts(_, [], ResultList) ->
	ResultList;
putConnCongfigsToSpouts(ConnConfigs,[H|T] = _SpoutsConfigs, ResultList)
  when is_record(H, spoutConfig)->	
	H1 = putConnsToSpout(ConnConfigs, H),
	putConnCongfigsToSpouts(ConnConfigs, T, [H1|ResultList]).


putConnsToSpout([H|T] = _ConnConfigs, SpoutConfig)
  when is_record(SpoutConfig, spoutConfig) ->
	SpoutConfig1 = putSingleConnToSpout(H, SpoutConfig),
	putConnsToSpout(T, SpoutConfig1);
putConnsToSpout([], SpoutConfig) -> SpoutConfig.	

putSingleConnToSpout(ConnConfig, SpoutConfig)
  when is_record(ConnConfig, connConfig) and is_record(SpoutConfig, spoutConfig) ->
    #connConfig{from = From, to = To, grouping = Grouping} = ConnConfig,
	if
		From == SpoutConfig#spoutConfig.id ->
			SpoutTo = SpoutConfig#spoutConfig.to,
			SpoutConfig#spoutConfig{to = [{To, Grouping}|SpoutTo]};
		true ->
			SpoutConfig
	end.
  

%% 将连接信息写入Bolts的From和To字段
putConnPairsToBolts(VerifiedConnCongfigsList, BoltsConfigs) ->
	putConnCongfigsToBolts(VerifiedConnCongfigsList, BoltsConfigs, []).

putConnCongfigsToBolts(_, [], ResultList) ->
	ResultList;
putConnCongfigsToBolts(ConnConfigs,[H|T] = _BoltsConfigs, ResultList)
  when is_record(H, boltConfig)->	
	H1 = putConnsToBolt(ConnConfigs, H),
	putConnCongfigsToBolts(ConnConfigs, T, [H1|ResultList]).


putConnsToBolt([H|T] = _ConnConfigs, BoltConfig)
  when is_record(BoltConfig, boltConfig) ->
	BoltConfig1 = putSingleConnToBolt(H, BoltConfig),
	putConnsToBolt(T, BoltConfig1);
putConnsToBolt([], BoltConfig) -> BoltConfig.	

putSingleConnToBolt(ConnConfig, BoltConfig)
  when is_record(ConnConfig, connConfig) and is_record(BoltConfig, boltConfig) ->
    #connConfig{from = From, to = To, grouping = Grouping} = ConnConfig,
	if
		From == BoltConfig#boltConfig.id ->
			BoltConfig#boltConfig{to = [{To, Grouping}|BoltConfig#boltConfig.to]};
		To == BoltConfig#boltConfig.id ->
			BoltConfig#boltConfig{from = [{From, Grouping}|BoltConfig#boltConfig.from]};
		true ->
			BoltConfig
	end.

%% 验证模块文件是否存在本地
varifyActorFilesReady(Topo) when is_record(Topo, topoConfig) ->
	Topo.