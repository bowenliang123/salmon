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
-export([a/0,d/0,b/0,bb/0,c/0,cc/0,f/0]).

%%
%% API Functions
%%

ex()->
	Topo = sm:newTopo("t1"),
	Topo1 = sm:setSpout(Topo, "A", ex_spout, 5),
%% 	Topo2 = sm:setSpout(Topo1, "B", ex_spout),
	Topo3 = sm:setBolt(Topo1, "C", ex_bolt,1),
%% 	Topo4 = sm:setBolt(Topo3, "D", ex_bolt1,2),
	Topo5 = sm:shuffleGrouping(Topo3, "A", "C"),
%% 	Topo6 = sm:shuffleGrouping(Topo5, "B", "C"),
%% 	Topo7 = sm:shuffleGrouping(Topo5, "C", "D"),
	io:format("Topo to submit:~n~p~n",[Topo5]),
	Cluster = sm:cluster("192.168.207.128", 2181),
	_FeedBack = sm:submitTopology(Cluster, Topo5).
	
xx()->
	application:start(sasl),
	application:start(ezk),
	toolbar:start(),
	ex:ex().

yy()->
	appmon:start(),
	application:start(salmon).

d()->
	b(),
	c().

b()->
	spawn(?MODULE,bb,[]).
bb()->
	ActorName=sm_utils:genServerName("t1", spout, "A", 0),
	R=gen_server:call(ActorName,{check,self()}),
	error_logger:info_msg("~p~p~n",[ActorName,R]),
	timer:sleep(1000),
	bb().

c()->
	spawn(?MODULE,cc,[]).
cc()->
	ActorName=sm_utils:genServerName("t1", bolt, "C", 0),
	R=gen_server:call(ActorName,{check,self()}),
	error_logger:info_msg("~p~p~n",[ActorName,R]),
	timer:sleep(1000),
	cc().

%% e()->
%% 	spawn(?MODULE,ee,[]).
%% ee()->
%% %% 	ActorName=sm_utils:genServerName("t1", spout, "A", 0),
%% 	R=gen_server:call(hi, check),
%% 	error_logger:info_msg("~p~p~n",[hi,R]),
%% 	timer:sleep(1000),
%% 	ee().

%% 	gen_server:cast(ActorName,{nextTuple,#tuple{content="lbw"},self()}).

a()->
	sm_zk:setTopoStatus("t1", ?TOPO_STATUS_READY).
%%
%% Local Functions
%%
f()->
	sm_zk:ls("/"),
	f().
