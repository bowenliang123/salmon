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
-export([get/1, set/2, create/2, ls/1, fs/2]).
-export([genPath/1, genPath/2, genPath/3, genPath/4]).

-export([ifEzkLaunched/0]).

%%
%% API Functions
%%

%% Check if an application already launched
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

get(Path) ->
	startEzk(),
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:get(Conn,Path),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,{Content_bin,_}} ->
			binary_to_term(Content_bin);
		{_,_} ->
			Response
	end.

set(Path, ContentTerm) ->
	startEzk(),
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:set(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			Response
	end.

create(Path, ContentTerm) ->
	startEzk(),
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:create(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			Response
	end.

fs(Path, ContentTerm) ->
	startEzk(),
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:set(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			zk:create(Path, term_to_binary(ContentTerm))
	end.

ls(Path) ->
	startEzk(),
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:ls(Conn,Path),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,ContentList_bin}->
			list_b2t(ContentList_bin);
		{_,_} ->
			Response
	end.



%% Generate Path to Znode
genPath(TopoId)->
	RootPath = "/topos",
	utils:concatStrs([RootPath,"/",TopoId]).

genPath(TopoId, Type) ->
	Path = genPath(TopoId), 
	case Type of
		spout->
			Path2 = utils:concatStrs([Path, "/spouts"]);
		bolt->
			Path2 = utils:concatStrs([Path, "/bolts"]);
		conn->
			Path2 = utils:concatStrs([Path, "/conns"]);
		
		spouts->
			Path2 = utils:concatStrs([Path, "/spouts"]);
		bolts->
			Path2 = utils:concatStrs([Path, "/bolts"]);
		conns->
			Path2 = utils:concatStrs([Path, "/conns"])
	end,
	Path2.

genPath(TopoId, Type, Name) ->
	Path = genPath(TopoId, Type),
	utils:concatStrs([Path, "/", Name]).

genPath(TopoId, Type, Name, Index) ->
	if  (Type == spouts) or (Type == bolts) or (Type == spout) or (Type == bolt) ->
		Path = genPath(TopoId, Type, Name),
		utils:concatStrs([Path,"/", Index])
	end.

%%
%% Local Functions
%%

%% Check if an application already launched
ifAppLaunched(AppName, []) ->
	false;
ifAppLaunched(AppName, [H|T] = AppList) ->
	io:format("~p~p~n",[AppName,AppList]),
	case H of
		{AppName, _, _} ->
			true;
		true ->
			ifAppLaunched(AppName, T)			
	end.
	
%% Transfer a List from binary item to list item
list_b2t([H|T])->
	list_b2t(T,[binary_to_list(H)]).
list_b2t([], ResultList)->
	ResultList;
list_b2t([H|T], ResultList) ->
	list_b2t(T, lists:append([ResultList,[binary_to_list(H)]])).