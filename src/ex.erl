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
-export([start/0,ex/0]).

%%
%% API Functions
%%
ex()->
	Dict1 = dict:new(),
	Dict2 = dict:append("key", "Value", Dict1),
	dict:is_key("key", Dict2).

start()->
	Topo = sardine:newTopo(),
	Topo1 = sardine:setSpout(Topo, "a", "b"),
	Topo2 = sardine:setSpout(Topo1, "c", "d",3),
	Topo3 = sardine:setBolt(Topo2, "e", "f",4),
	Topo4 = sardine:shuffleGrouping(Topo3, "c", "e"),
	Topo5 = sardine:shuffleGrouping(Topo4, "", "f"),
	io:format("~p~n", [Topo5]),
	Cluster = sardine:cluster("127.0.0.1", 2181),
	FeedBack = sardine:submitTopology(Cluster, Topo5).
	

%%
%% Local Functions
%%

