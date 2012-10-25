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
	travseWorkers(TopoId,SpoutName,SpoutPath,SpoutCount),
	ok.

travseWorkers(TopoId, SpoutName, SpoutPath, WorkerCount) ->
	if WorkerCount > 0 ->
			SingleTypeSpoutIndex = WorkerCount - 1,
			SpoutNumPath = utils:concatStrs([SpoutPath, "/", SingleTypeSpoutIndex]),
			SingleSpoutInfo = utils:zkget(SpoutNumPath),
			IsServerReady = checkServerReady(SingleSpoutInfo),
			if 
				IsServerReady /= true ->
				  setupSpoutServer(TopoId,SpoutName,SingleTypeSpoutIndex);
				true ->
					io:format("spoutSever has been already set!～n"),
					setupSpoutServer(TopoId,SpoutName,SingleTypeSpoutIndex)
			end,
			
			travseWorkers(TopoId, SpoutName, SpoutPath, WorkerCount - 1);
		  true ->
			  ok
	end.

checkServerReady(SingleInfo) ->
	if 
		(SingleInfo#worker_info.self_name == null_server) or (SingleInfo#worker_info.node_name == null_node) ->
			false;
		true ->
			true
	end.

setupSpoutServer(TopoId,SpoutTypeName,SingleTypeSpoutIndex) ->
	io:format("~p~p~p~n", [TopoId,SpoutTypeName,SingleTypeSpoutIndex]),
	spout_server:start_link(TopoId,SpoutTypeName,SingleTypeSpoutIndex).
