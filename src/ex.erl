%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to ex
-module(ex).

%%
%% Include files
%%
-include("../include/tuple.hrl").
%%
%% Exported Functions
%%
-export([ex/0]).
-export([xx/0]).
-export([aa/0]).

%%
%% API Functions
%%

ex()->
	Topo = sm:newTopo("t1"),
	Topo1 = sm:setSpout(Topo, "A", moduleA),
	Topo2 = sm:setSpout(Topo1, "B", moduleB),
	Topo3 = sm:setBolt(Topo2, "C", ex_bolt),
	Topo4 = sm:shuffleGrouping(Topo3, "A", "C"),
	Topo5 = sm:shuffleGrouping(Topo4, "B", "C"),
	io:format("Topo to submit:~n~p~n",[Topo5]),
	Cluster = sm:cluster("192.168.207.128", 2181),
	FeedBack = sm:submitTopology(Cluster, Topo5).
	
xx()->
	appmon:start(),
	application:start(sasl),
	ex:ex(),
	application:start(salmon).

aa()->
	ServerName=sm_utils:genServerName("t1", bolt, "C", 0),
	gen_server:cast(ServerName,{nextTuple,#tuple{content="lbw"}}).
%%
%% Local Functions
%%

