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
-export([start/0,listen_spouts/1,listen_bolts/1]).
-export([hi/0]).


%%
%% API Functions
%%
hi()->
	io:format("it is hi~n").

start() ->
	application:start(ezk),
	TopoId="topo1",
%% 	listen_spouts(TopoId),
	listen_bolts(TopoId),
%% 	spawn(test,listen_spouts,[]),
%% 	spawn(test,listen_bolts,[]),
	ok.

listen_spouts(TopoId) ->	
	SpoutsPath = zkpath:genPath(TopoId, spouts),
	SpoutsList = utils:zkget(SpoutsPath),
	io:format("~p~n", [SpoutsList]),
	
	travse(TopoId, spouts, SpoutsList),
	ok.


listen_bolts(TopoId)->
	SpoutsPath = zkpath:genPath(TopoId, bolts),
	SpoutsList = utils:zkget(SpoutsPath),
	io:format("~p~n", [SpoutsList]),
	
	travse(TopoId, bolts, SpoutsList),
	ok.




%%
%% Local Functions
%%
travse(TopoId,Type, []) ->
	ok;
travse(TopoId, Type, [H|T] = SpoutNameList) ->
	travse2(TopoId, Type, H),
	travse(TopoId, Type, T).


travse2(TopoId, Type, SpoutName) ->
	
	SpoutPath = zkpath:genPath(TopoId, Type, SpoutName),
	io:format("SP~p~n", [SpoutPath]),
	SpoutInfo = utils:zkget(SpoutPath),
	SpoutCount = SpoutInfo#type_info.count,
	
	io:format("~p~p~p~n", [SpoutName,SpoutCount,SpoutPath]),
	checkWorker(TopoId, Type, SpoutName, SpoutCount - 1),
	ok.


checkWorker(_,_,_,-1) ->
	ok;
checkWorker(TopoId, Type, SpoutName, Index) ->
   WorkerPath = zkpath:genPath(TopoId, Type, SpoutName, Index),
   SingleSpoutInfo = utils:zkget(WorkerPath),
   IsServerReady = checkWorkerReady(SingleSpoutInfo),
   if
	   IsServerReady /= true ->
	     setupWorker(TopoId, Type, SpoutName, Index);
	   true ->
		   io:format("spoutSever has been already set!～n"),
		   setupWorker(TopoId, Type, SpoutName, Index)
   end,

   checkWorker(TopoId, Type, SpoutName, Index - 1).

checkWorkerReady(WorkerInfo) ->
	#worker_info{self_name = Server,node_name = Node} = WorkerInfo,
	if
		(Server == null_server) or (Node == null_node) ->
			false;
		true ->
			true
	end.

setupWorker(TopoId, Type, Name, Index) ->
	io:format("Try to setup Server.~n(TopoId:~p,Type:~p,Name:~p,Index:~p)~n", [TopoId,Type,Name,Index]),
	case Type of
		spout ->
			spout_worker:start_link(TopoId,Name,Index);
		bolt ->
			bolt_worker:start_link(TopoId,Name,Index);
		spouts ->
			spout_worker:start_link(TopoId,Name,Index);
		bolts ->
			bolt_worker:start_link(TopoId,Name,Index)
	end.
