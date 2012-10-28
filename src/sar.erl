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
	io:format("Listen_Spouts:~p~n", [TopoId]),
	SpoutsPath = zk:genPath(TopoId, spouts),
	SpoutsList = zk:get(SpoutsPath),
	io:format("SpoutsList:~p~n", [SpoutsList]),
	
	travse(TopoId, spouts, SpoutsList),
	ok.


listen_bolts(TopoId)->
	io:format("Listen_Bolts:~p~n", [TopoId]),
	BoltsPath = zk:genPath(TopoId, bolts),
	BoltsList = zk:get(BoltsPath),
	io:format("BoltsList:~p~n", [BoltsList]),
	
	travse(TopoId, bolts, BoltsList),
	ok.




%%
%% Local Functions
%%
travse(TopoId,Type, []) ->
	ok;
travse(TopoId, Type, [H|T] = SpoutNameList) ->
	travse2(TopoId, Type, H),
	travse(TopoId, Type, T).


travse2(TopoId, Type, Name) ->
	
	SpoutPath = zk:genPath(TopoId, Type, Name),
	io:format("SP~p~n", [SpoutPath]),
	SpoutInfo = zk:get(SpoutPath),
	SpoutCount = SpoutInfo#type_info.count,
	
	io:format("~p~p~p~n", [Name,SpoutCount,SpoutPath]),
	checkWorker(TopoId, Type, Name, SpoutCount - 1),
	ok.


checkWorker(_,_,_,-1) ->
	ok;
checkWorker(TopoId, Type, Name, Index) ->
   WorkerPath = zk:genPath(TopoId, Type, Name, Index),
   WorkerInfo = zk:get(WorkerPath),
   IsServerReady = checkWorkerReady(WorkerInfo),
   if
	   IsServerReady /= true ->
	     setupWorker(TopoId, Type, Name, Index);
	   true ->
		   io:format("spoutSever has been already set!～n"),
		   setupWorker(TopoId, Type, Name, Index)
   end,

   checkWorker(TopoId, Type, Name, Index - 1).

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
