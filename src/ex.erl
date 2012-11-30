%% Author: Administrator
%% Created: 2012-11-15
%% Description: TODO: Add description to ex
-module(ex).

%%
%% Include files
%%
-include("../include/tuple.hrl").
-include("../include/sm_topostatus.hrl").
%%
%% Exported Functions
%%
-export([ex/0]).
-export([xx/0,yy/0]).
-export([aa/0,bb/0]).

%%
%% API Functions
%%

ex()->
	Topo = sm:newTopo("t1"),
	Topo1 = sm:setSpout(Topo, "A", ex_spout),
%% 	Topo2 = sm:setSpout(Topo1, "B", ex_spout),
	Topo3 = sm:setBolt(Topo1, "C", ex_bolt),
%% 	Topo4 = sm:setBolt(Topo3, "D", ex_bolt1,2),
	Topo5 = sm:shuffleGrouping(Topo3, "A", "C"),
%% 	Topo6 = sm:shuffleGrouping(Topo5, "B", "C"),
%% 	Topo7 = sm:shuffleGrouping(Topo5, "C", "D"),
	io:format("Topo to submit:~n~p~n",[Topo5]),
	Cluster = sm:cluster("192.168.207.128", 2181),
	_FeedBack = sm:submitTopology(Cluster, Topo5).
	
xx()->
	application:start(sasl),
	ex:ex().

yy()->
	appmon:start(),
	application:start(salmon).

aa()->
	ActorName=sm_utils:genServerName("t1", spout, "A", 0),
	gen_server:call(ActorName,{nextTuple, self()}).
%% 	gen_server:cast(ActorName,{nextTuple,#tuple{content="lbw"},self()}).

bb()->
	sm_zk:setTopoStatus("t1", ?TOPO_STATUS_READY).
%%
%% Local Functions
%%

