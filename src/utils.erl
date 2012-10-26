%% Author: simonlbw
%% Created: 2012-10-24
%% Description: TODO: Add description to utils
-module(utils).

%%
%% Include files
%%
-include("../include/zkdata_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([concatStrs/1]).
-export([file_exist/1]).
-export([zkget/1, zkset/2, zkcreate/2, zkls/1, zkfs/2]).
-export([genServerName/4]).
-export([getModule/3]).
-export([init/0]).
%%
%% API Functions
%%
init()->
	application:start(ezk),
	TopoId = "topo1",
	Path1 = zkpath:genPath(TopoId),
	utils:zkfs(Path1, ""),
	
	Path2 = zkpath:genPath(TopoId,spouts),
	utils:zkfs(Path2, ["Producer"]),
	
	Path3 = zkpath:genPath(TopoId,bolts),
	utils:zkfs(Path3, ["Consumer"]),
	
	Path99 = zkpath:genPath(TopoId,conns),
	utils:zkfs(Path99, ["Producer"]),
	
	Path4 = zkpath:genPath(TopoId,spouts,"Producer"),
	utils:zkfs(Path4, #type_info{module=examplespout,count=1}),
	
	Path5 = zkpath:genPath(TopoId,bolts,"Consumer"),
	utils:zkfs(Path5, #type_info{module=examplebolt,count=1}),
	
	Path6 = zkpath:genPath(TopoId,spouts,"Producer",0),
	utils:zkfs(Path6, #worker_info{}),
	
	Path7 = zkpath:genPath(TopoId,bolts,"Consumer",0),
	utils:zkfs(Path7, #worker_info{}),
	
	Path8 = zkpath:genPath(TopoId, conns, "Producer"),
	utils:zkfs(Path8, ["Consumer"]),
	
	ok.


concatStrs(Str) ->
	concatStrs(Str,"").

file_exist(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         -> io:format("~s is found~n", [Filename]);
        {error, enoent} -> io:format("~s is missing~n", [Filename]);
        {error, Reason} -> io:format("~s is ~s~n", [Filename, Reason])
    end.

zkget(Path) ->
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:get(Conn,Path),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,{Content_bin,_}} ->
			binary_to_term(Content_bin);
		{_,_} ->
			Response
	end.

zkset(Path, ContentTerm) ->
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:set(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			Response
	end.

zkcreate(Path, ContentTerm) ->
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:create(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			Response
	end.

zkfs(Path, ContentTerm) ->
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:set(Conn, Path, term_to_binary(ContentTerm)),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,_} -> 
			{ok,set_successfully};
		{_,_} ->
			zkcreate(Path, term_to_binary(ContentTerm))
	end.

zkls(Path) ->
	{ok,Conn} = ezk:start_connection(),
	Response = ezk:ls(Conn,Path),
	ezk:end_connection(Conn,""),
	case Response of
		{ok,ContentList_bin}->
			list_b2t(ContentList_bin);
		{_,_} ->
			Response
	end.


genServerName(Type,TopoId,SpoutTypeName,Index) ->
	case Type of
		spout->
			ServerName = utils:concatStrs(["spout",TopoId,SpoutTypeName,Index]);
		bolt->
			ServerName = utils:concatStrs(["bolt",TopoId,SpoutTypeName,Index])
	end,
	list_to_atom(ServerName).



getModule(Type, TopoId, Name) ->
	Path = zkpath:genPath(TopoId, Type, Name),
	SpoutTypeInfo = utils:zkget(Path),
	io:format("TT:~p~p~p~n", [Type,Path,SpoutTypeInfo]),
	Module = SpoutTypeInfo#type_info.module,
	Module.

%%
%% Local Functions
%%

concatStrs([],ResultStr)->
	ResultStr;
concatStrs([H|T], ResultStr) ->
	if
		is_number(H) ->
			concatStrs(T,string:concat(ResultStr, integer_to_list(H)));
		is_atom(H) ->
			concatStrs(T,string:concat(ResultStr, atom_to_list(H)));
		true->
			concatStrs(T,string:concat(ResultStr, H))
	end.


list_b2t([H|T])->
	list_b2t(T,[binary_to_list(H)]).
list_b2t([], ResultList)->
	ResultList;
list_b2t([H|T], ResultList) ->
	list_b2t(T, lists:append([ResultList,[binary_to_list(H)]])).

	
