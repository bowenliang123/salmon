%% Author: Administrator
%% Created: 2012-11-21
%% Description: TODO: Add description to sardine_publisher
-module(sardine_publisher).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").
%%
%% Exported Functions
%%
-export([publishTopology/2]).

%%
%% API Functions
%%
publishTopology(Cluster, Topo)
	when is_record(Cluster, clusterConfig) and is_record(Topo, topoConfig) ->
		#topoConfig{id = TopoId} = Topo,
		publishTopo(Cluster, TopoId,Topo),
		publishSpouts(Cluster, TopoId,Topo),
		publishBolts(Cluster, TopoId,Topo).
		
		
		

%%
%% Local Functions
%%
publishTopo(Cluster, TopoId, Topo) when is_record(Topo, topoConfig)->
	{ok, ConnPId} = getTempConn(Cluster),
	{ok, OriginalToposIdList} = zk:get(ConnPId, "/topos"),
	NewToposList = addToToposIdList(OriginalToposIdList, TopoId),
	if NewToposList=/= OriginalToposIdList->
		   zk:set(ConnPId, "/topos", NewToposList);
		  true -> ok
	end,
	Path = zk:genPath(TopoId),
	zk:delete_all(ConnPId, Path),
	zk:create(ConnPId, Path, Topo),
	ezk:end_connection(ConnPId, "initialTopo").

publishSpouts(Cluster, TopoId, Topo) when is_record(Topo, topoConfig)->
	{ok, ConnPId} = getTempConn(Cluster),
	SpoutsConfigList = Topo#topoConfig.spouts,
	initialSpouts(ConnPId, TopoId, SpoutsConfigList),
	ezk:end_connection(ConnPId, "initialTopo").

publishBolts(Cluster, TopoId, Topo) when is_record(Topo, topoConfig)->
	{ok, ConnPId} = getTempConn(Cluster),
	BoltsConfigList = Topo#topoConfig.bolts,
	initialBolts(ConnPId, TopoId, BoltsConfigList),
	ezk:end_connection(ConnPId, "initialTopo").

%% initial Spouts
initialSpouts(ConnPId, TopoId, [H|T] = _SpoutsConfigList)
  when is_record(H, spoutConfig)->
	#spoutConfig{id = SpoutId, count = Count} = H,
	case zk:get(ConnPId, zk:genPath(TopoId, spouts)) of
		{ok, OldSpoutsIdList} ->
			case sardine_common:isInList(SpoutId, OldSpoutsIdList) of
				false->
					NewSpoutsIdList = [SpoutId, OldSpoutsIdList];
				true->
					NewSpoutsIdList = OldSpoutsIdList
			end,
			zk:replace(ConnPId, zk:genPath(TopoId, spouts), NewSpoutsIdList);
		_->
			zk:replace(ConnPId, zk:genPath(TopoId, spouts), [])
	end,	
	Path = zk:genPath(TopoId, spout, SpoutId),
	zk:replace(ConnPId, Path, H),
	initialSpoutChildren(ConnPId, TopoId, SpoutId, Count),
	initialSpouts(ConnPId, TopoId, T);
initialSpouts(_, _, []) ->
	ok.

initialSpoutChildren(ConnPId, TopoId, SpoutId, Count) when (Count>=0) ->
	initialSpoutChildren(ConnPId, TopoId, SpoutId, Count, 0).
  
initialSpoutChildren(ConnPId, TopoId, SpoutId, Count, Index)
  when (Count>=0) and (Index>=0) and (Index<Count) ->
	Path = zk:genPath(TopoId, spout, SpoutId, Index),
	zk:create(ConnPId, Path, null),
	initialSpoutChildren(ConnPId, TopoId, SpoutId, Count, Index+1);
initialSpoutChildren(_,_,_,_,_) ->
	ok.

%% initial Bolts
initialBolts(ConnPId, TopoId, [H|T] = _BoltsConfigList)
  when is_record(H, boltConfig)->
	#boltConfig{id = BoltId, count = Count} = H,
	case zk:get(ConnPId, zk:genPath(TopoId, bolts)) of
		{ok, OldBoltsIdList} ->
			case sardine_common:isInList(BoltId, OldBoltsIdList) of
				false->
					NewBoltsIdList = [BoltId, OldBoltsIdList];
				true->
					NewBoltsIdList = OldBoltsIdList
			end,
			zk:replace(ConnPId, zk:genPath(TopoId, bolts), NewBoltsIdList);
		_->
			zk:replace(ConnPId, zk:genPath(TopoId, bolts), [])
	end,	
	Path = zk:genPath(TopoId, bolts, BoltId),
	zk:replace(ConnPId, Path, H),
	initialBoltChildren(ConnPId, TopoId, BoltId, Count),
	initialBolts(ConnPId, TopoId, T);
initialBolts(_, _, []) ->
	ok.

initialBoltChildren(ConnPId, TopoId, BoltId, Count) when (Count>=0) ->
	initialBoltChildren(ConnPId, TopoId, BoltId, Count, 0).
  
initialBoltChildren(ConnPId, TopoId, BoltId, Count, Index)
  when (Count>=0) and (Index>=0) and (Index<Count) ->
	Path = zk:genPath(TopoId, bolt, BoltId, Index),
	zk:create(ConnPId, Path, null),
	initialBoltChildren(ConnPId, TopoId, BoltId, Count, Index+1);
initialBoltChildren(_,_,_,_,_) ->
	ok.


addToToposIdList(ToposList, TopoId) when is_list(ToposList) ->
	case sardine_common:isInList(TopoId, ToposList) of
		true->
			ToposList;
		false->
			[TopoId|ToposList]
	end.
	

getTempConn(Cluster) when is_record(Cluster, clusterConfig)->
	#clusterConfig{zkip = ZkIp, port = Port} = Cluster,
	ZkServer = {ZkIp, Port, 30000, 100000},
	{ok, ConnPid} = zk:startConnection([ZkServer]).
