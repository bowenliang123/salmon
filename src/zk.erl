%% Author: Administrator
%% Created: 2012-10-28
%% Description: TODO: Add description to zk
-module(zk).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-export([get/1, set/2, create/2, ls/1, replace/2, exists/1, delete_all/1]).
-export([get/2, set/3, create/3, ls/2, replace/3, exists/2, delete_all/2]).
-export([genPath/1, genPath/2, genPath/3, genPath/4]).

-export([ifEzkLaunched/0]).
-export([getConnection/0, startConnection/1]).

%%
%% API Functions
%%

%% Check if an application already launched
getConnection() ->
	startEzk(),
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
	{ok,ConnPid} =  zk:getConnection(),
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
	{ok,ConnPid} =  zk:getConnection(),
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
	{ok,ConnPid} = zk:getConnection(),
	set(ConnPid, Path, ContentTerm).

set(ConnPid, Path, ContentTerm) when is_pid(ConnPid) ->
	Response = ezk:set(ConnPid, Path, term_to_binary(ContentTerm)),
	case Response of
		{ok, _} -> 
			{ok, Path, ContentTerm};
		{_, _} ->
			Response
	end.


create(Path, ContentTerm) ->
	{ok,ConnPid} = zk:getConnection(),
	create(ConnPid, Path, ContentTerm).

create(ConnPid, Path, ContentTerm) when is_pid(ConnPid) ->
	Response = ezk:create(ConnPid, Path, term_to_binary(ContentTerm)),
	case Response of
		{ok, _} -> 
			{ok,Path, ContentTerm};
		{_, _} ->
			Response
	end.


replace(Path, ContentTerm) ->
	{ok,ConnPid} = zk:getConnection(),
	replace(ConnPid, Path, ContentTerm).

replace(ConnPid, Path, ContentTerm) when is_pid(ConnPid)->
	case zk:exists(ConnPid, Path) of
		false->
			Response = zk:create(ConnPid, Path, ContentTerm);
		true->
			Response = zk:set(ConnPid, Path, ContentTerm)
	end,
	case Response of
		{ok, _} -> 
			Response;
		_ ->
			Response
	end.

ls(Path) ->
	{ok,ConnPid} = zk:getConnection(),
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
	{ok,ConnPid} = zk:getConnection(),
	delete_all(ConnPid, Path).

delete_all(ConnPid, Path) when is_pid(ConnPid)->
	_Response = ezk:delete_all(ConnPid, Path).
			


%% Generate Path to Znode
genPath(TopoId)->
	RootPath = "/topos",
	sardine_utils:concatStrs([RootPath,"/",TopoId]).

genPath(TopoId, Type) ->
	Path = genPath(TopoId), 
	case Type of
		spout->
			Path2 = sardine_utils:concatStrs([Path, "/spouts"]);
		bolt->
			Path2 = sardine_utils:concatStrs([Path, "/bolts"]);
		conn->
			Path2 = sardine_utils:concatStrs([Path, "/conns"]);
		
		spouts->
			Path2 = sardine_utils:concatStrs([Path, "/spouts"]);
		bolts->
			Path2 = sardine_utils:concatStrs([Path, "/bolts"]);
		conns->
			Path2 = sardine_utils:concatStrs([Path, "/conns"])
	end,
	Path2.

genPath(TopoId, Type, Name) ->
	Path = genPath(TopoId, Type),
	sardine_utils:concatStrs([Path, "/", Name]).

genPath(TopoId, Type, Name, Index) ->
	if  (Type == spouts) or (Type == bolts) or (Type == spout) or (Type == bolt) ->
		Path = genPath(TopoId, Type, Name),
		sardine_utils:concatStrs([Path, "/", Index])
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