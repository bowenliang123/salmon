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
-export([start/0]).

%%
%% API Functions
%%

start()->
	Topo = sardine:newTopo(),
	Topo1 = sardine:setSpout(Topo, "a", "b"),
	Topo2 = sardine:setSpout(Topo1, "c", "d",3),
	Topo3 = sardine:setBolt(Topo2, "e", "f",4),
	Topo4 = sardine:shuffleGrouping(Topo3, "c", "e").
	

%%
%% Local Functions
%%

