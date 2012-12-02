%% Author: Administrator
%% Created: 2012-10-28
%% Description: TODO: Add description to zk
-module(sm_zk).

%%
%% Include files
%%
-include("../include/sm_topostatus.hrl").
%%
%% Exported Functions
%%
-export([]).
-export([get/1, set/2, create/2, ls/1, replace/2, exists/1, delete_all/1]).
-export([get/2, set/3, create/3, ls/2, replace/3, exists/2, delete_all/2]).
-export([rootPath/0]).
-export([genPath/1, genPath/2, genPath/3, genPath/4]).

-export([getTopoStatus/1,setTopoStatus/2]).

-export([ifEzkLaunched/0]).
-export([getConnection/0, startConnection/1]).


-define(ROOT_PATH,"/topos").

%%
%% API Functions
%%

%% Basic low-Level Zookeeper operations

%% Check if an application already launched
getConnection() ->
%% 	startEzk(),
	{ok, Connections} = ezk:get_connections(),
	case Connections of
		[{ConnPid,_}|_]->
			{ok, ConnPid};
		[]->
			{ok, _ConnPid} = ezk:start_connection()
	end.

startConnection(Servers) when is_list(Servers)->
	startEzk(),
	{ok, _ConnPid} = ezk:start_connection(Servers).

ifEzkLaunched() ->
	RunningApplicationsList = application:which_applications(),
	ifAppLaunched(ezk,RunningApplicationsList).

%% Start Ezk if not started yet
startEzk() ->
	IfEzkLaunched = ifEzkLaunched(),
	case IfEzkLaunched of 
		   false->
				application:start(ezk);
		   true ->
				ok
	end.

exists(Path) ->
	{ok,ConnPid} =  sm_zk:getConnection(),
	exists(ConnPid, Path).

exists(ConnPid, Path) when is_pid(ConnPid) ->
	Response = ezk:exists(ConnPid,Path),
	case Response of
		{ok, _} ->
			true;
		{error,no_dir} ->
			false;
		_ ->
			false
	end.


get(Path) ->
	{ok,ConnPid} =  sm_zk:getConnection(),
	get(ConnPid, Path).

get(ConnPid, Path) when is_pid(ConnPid) ->
	Response = ezk:get(ConnPid,Path),	
	case Response of
		{ok,{<<>>,_}}->
			{ok, ""};
		{ok,{Content_bin,_}} ->
			{ok, binary_to_term(Content_bin)};
		{error,_} ->
			Response
	end.


set(Path, ContentTerm) ->
	{ok,ConnPid} = sm_zk:getConnection(),
	set(ConnPid, Path, ContentTerm).

set(ConnPid, Path, ContentTerm) when is_pid(ConnPid) ->
	Response = ezk:set(ConnPid, Path, term_to_binary(ContentTerm)),
	case Response of
		{ok, _} -> 
			{ok, {Path, ContentTerm}};
		{_, _} ->
			Response
	end.


create(Path, ContentTerm) ->
	{ok,ConnPid} = sm_zk:getConnection(),
	create(ConnPid, Path, ContentTerm).

create(ConnPid, Path, ContentTerm) when is_pid(ConnPid) ->
	Response = ezk:create(ConnPid, Path, term_to_binary(ContentTerm)),
	case Response of
		{ok, _} -> 
			{ok,{Path, ContentTerm}};
		{_, _} ->
			Response
	end.


replace(Path, ContentTerm) ->
	{ok,ConnPid} = sm_zk:getConnection(),
	replace(ConnPid, Path, ContentTerm).

replace(ConnPid, Path, ContentTerm) when is_pid(ConnPid)->
	case sm_zk:exists(ConnPid, Path) of
		false->
			Response = sm_zk:create(ConnPid, Path, ContentTerm);
		true->
			Response = sm_zk:set(ConnPid, Path, ContentTerm)
	end,
	case Response of
		{ok, _} -> 
			Response;
		_ ->
			Response
	end.

ls(Path) ->
	{ok,ConnPid} = sm_zk:getConnection(),
	ls(ConnPid, Path).

ls(ConnPid, Path) when is_pid(ConnPid) ->
	Response = ezk:ls(ConnPid,Path),
	case Response of
		{ok,ContentList_bin}->
			{ok, list_b2t(ContentList_bin)};
		{_,_} ->
			Response
	end.

delete_all(Path) ->
	{ok,ConnPid} = sm_zk:getConnection(),
	delete_all(ConnPid, Path).

delete_all(ConnPid, Path) when is_pid(ConnPid)->
	_Response = ezk:delete_all(ConnPid, Path).
		
%% High-level zk operations
getTopoStatus(TopoId)->
	Path = sm_utils:concatStrs([sm_zk:genPath(TopoId), "/", status]),
	{ok, Status} = sm_zk:get(Path),
	Status.

setTopoStatus(TopoId, Status) when is_atom(Status)->
	Path = sm_utils:concatStrs([sm_zk:genPath(TopoId), "/", status]),
	case sm_utils:isInList(Status, ?TOPO_STATUS_LIST) of
		true->
			{ok, R} = sm_zk:replace(Path, Status),
			{ok, R};
		false->
			{error, not_valid_topo_status}
	end.
	


%% Generate Path to Znode
rootPath()->
	?ROOT_PATH.

genPath(TopoId)->
	sm_utils:concatStrs([?ROOT_PATH,"/",TopoId]).

genPath(TopoId, Type) ->
	Path = genPath(TopoId), 
	case Type of
		spout->
			Path2 = sm_utils:concatStrs([Path, "/spouts"]);
		bolt->
			Path2 = sm_utils:concatStrs([Path, "/bolts"]);
		conn->
			Path2 = sm_utils:concatStrs([Path, "/conns"]);
		
		spouts->
			Path2 = sm_utils:concatStrs([Path, "/spouts"]);
		bolts->
			Path2 = sm_utils:concatStrs([Path, "/bolts"]);
		conns->
			Path2 = sm_utils:concatStrs([Path, "/conns"])
	end,
	Path2.

genPath(TopoId, Type, Name) ->
	Path = genPath(TopoId, Type),
	sm_utils:concatStrs([Path, "/", Name]).

genPath(TopoId, Type, Name, Index) ->
	if  (Type == spouts) or (Type == bolts) or (Type == spout) or (Type == bolt) ->
		Path = genPath(TopoId, Type, Name),
		sm_utils:concatStrs([Path, "/", Index])
	end.

%%
%% Local Functions
%%

%% Check if an application already launched

ifAppLaunched(AppName, [H|T] = _AppNameList) ->
	case H of
		{AppName, _, _} ->
			true;
		_ ->
			ifAppLaunched(AppName, T)			
	end;
ifAppLaunched(_AppName, []) ->
	false.
	
%% Transfer a List from binary item to list item
list_b2t([H|T]) ->
	list_b2t(T,[binary_to_list(H)]);
list_b2t([]) ->
	[].

list_b2t([H|T], ResultList) ->
	list_b2t(T, lists:append([ResultList,[binary_to_list(H)]]));
list_b2t([], ResultList)->
	ResultList.