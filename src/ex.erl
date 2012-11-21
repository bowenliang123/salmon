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

%%
%% API Functions
%%

ex()->
	Topo = sardine:newTopo("t1"),
	Topo1 = sardine:setSpout(Topo, "a", "b"),
	Topo2 = sardine:setSpout(Topo1, "c", "d",3),
	Topo3 = sardine:setBolt(Topo2, "e", "f",4),
	Topo4 = sardine:shuffleGrouping(Topo3, "c", "e"),
	Topo5 = sardine:shuffleGrouping(Topo4, "a", "e"),
	Topo6 = sardine:setBolt(Topo5, "g", "h",4),
	Topo7 = sardine:shuffleGrouping(Topo6, "e", "g"),
	io:format("Topo to submit:~n~p~n",[Topo7]),
	Cluster = sardine:cluster("192.168.204.128", 2181),
	FeedBack = sardine:submitTopology(Cluster, Topo7).
	

%%
%% Local Functions
%%

