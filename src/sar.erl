%% Author: simonlbw
%% Created: 2012-10-22
%% Description: TODO: Add description to test
-module(sar).

%%
%% Include files
%%
-include("../include/zkdata_interface.hrl").

%%
%% Exported Functions
%%
-export([start/0,listen_spouts/1,listen_bolts/0]).
-export([hi/0]).


%%
%% API Functions
%%
hi()->
	io:format("it is hi~n").

start() ->
	application:start(ezk),
	TopoId="topo1",
	listen_spouts(TopoId),
%% 	spawn(test,listen_spouts,[]),
%% 	spawn(test,listen_bolts,[]),
	ok.

listen_spouts(TopoId) ->
	
	SpoutsPath = zkpath:genPath(TopoId, spouts),
	SpoutsList = utils:zkget(SpoutsPath),
	io:format("~p~n", [SpoutsList]),
	
	travseSpouts(TopoId, SpoutsPath, SpoutsList),
 ok.


listen_bolts()->
	
	ok.




%%
%% Local Functions
%%
travseSpouts(TopoId, SpoutsRootPath, []) ->
	ok;
travseSpouts(TopoId, SpoutsRootPath, [H|T] = SpoutTypeList) ->
	travseSingleSpout(TopoId, SpoutsRootPath, H),
	travseSpouts(TopoId,SpoutsRootPath,T),
	ok.


travseSingleSpout(TopoId, SpoutsRootPath, SpoutName) ->
	
	SpoutPath = zkpath:genPath(TopoId, spouts, SpoutName),
	SpoutInfo = utils:zkget(SpoutPath),
	SpoutCount = SpoutInfo#type_info.count,
	
	io:format("~p~p~p~n", [SpoutName,SpoutCount,SpoutPath]),
	travseSingleSpoutByNum(TopoId,SpoutName,SpoutPath,SpoutCount),
	ok.

travseSingleSpoutByNum(TopoId, SpoutTypeName, SpoutTypePath, SingleTypeSpoutCount) ->
	if SingleTypeSpoutCount > 0 ->
			SingleTypeSpoutIndex = SingleTypeSpoutCount - 1,
			SpoutNumPath = utils:concatStrs([SpoutTypePath, "/", SingleTypeSpoutIndex]),
			SingleSpoutInfo = utils:zkget(SpoutNumPath),
			IsServerSet = checkServer(SingleSpoutInfo),
			if 
				IsServerSet /= true ->
				  setupSpoutServer(TopoId,SpoutTypeName,SingleTypeSpoutIndex);
				true ->
					io:format("spoutSever has been already set!～n"),
					setupSpoutServer(TopoId,SpoutTypeName,SingleTypeSpoutIndex)
			end,
			
			travseSingleSpoutByNum(TopoId, SpoutTypeName, SpoutTypePath, SingleTypeSpoutCount - 1);
		  true ->
			  ok
	end.

checkServer(SingleInfo) ->
	if 
		(SingleInfo#worker_info.self_name == null_server) or (SingleInfo#worker_info.node_name == null_node) ->
			false;
		true ->
			true
			
	end.

setupSpoutServer(TopoId,SpoutTypeName,SingleTypeSpoutIndex) ->
	io:format("~p~p~p~n", [TopoId,SpoutTypeName,SingleTypeSpoutIndex]),
	spout_server:start_link(TopoId,SpoutTypeName,SingleTypeSpoutIndex).
