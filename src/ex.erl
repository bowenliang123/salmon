%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to ex
-module(ex).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([ex/0]).
-export([xx/0]).

%%
%% API Functions
%%

ex()->
	Topo = sm:newTopo("t1"),
	Topo1 = sm:setSpout(Topo, "a", "b"),
	Topo2 = sm:setSpout(Topo1, "c", "d", 3),
	Topo3 = sm:setBolt(Topo2, "e", "f", 4),
	Topo4 = sm:shuffleGrouping(Topo3, "c", "e"),
	Topo5 = sm:shuffleGrouping(Topo4, "a", "e"),
	Topo6 = sm:setBolt(Topo5, "g", "h", 4),
	Topo7 = sm:shuffleGrouping(Topo6, "e", "g"),
	io:format("Topo to submit:~n~p~n",[Topo7]),
	Cluster = sm:cluster("192.168.207.128", 2181),
	FeedBack = sm:submitTopology(Cluster, Topo7).
	
xx()->
	appmon:start(),
	application:start(sasl),
	ex:ex(),
	application:start(salmon).
%%
%% Local Functions
%%

