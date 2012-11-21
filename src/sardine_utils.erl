%% Author: simonlbw
%% Created: 2012-10-24
%% Description: TODO: Add description to utils
-module(sardine_utils).

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
-export([genServerName/4]).
-export([getModule/3]).
-export([init/0]).
-export([getToWorkerList/1, getDataFromState/1]).
%%
%% API Functions
%%
init()->
	ok.


concatStrs(Str) ->
	concatStrs(Str,"").

file_exist(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         -> io:format("~s is found~n", [Filename]);
        {error, enoent} -> io:format("~s is missing~n", [Filename]);
        {error, Reason} -> io:format("~s is ~s~n", [Filename, Reason])
    end.




genServerName(Type,TopoId,SpoutTypeName,Index) ->
	case Type of
		spout->
			ServerName = sardine_utils:concatStrs(["spout",TopoId,SpoutTypeName,Index]);
		bolt->
			ServerName = sardine_utils:concatStrs(["bolt",TopoId,SpoutTypeName,Index])
	end,
	list_to_atom(ServerName).



getModule(Type, TopoId, Name) ->
	Path = zk:genPath(TopoId, Type, Name),
	SpoutTypeInfo = zk:get(Path),
	Module = SpoutTypeInfo#type_info.module,
	Module.


getToWorkerList(SelfServerName) ->
	State = gen_server:call({global,SelfServerName}, getServerState),
	
	{TopoId, _Type, Name} = getDataFromState(State),
	ToList = zk:get(zk:genPath(TopoId, conns, Name)),
	case ToList of
		{error,_} ->
			[];
		 _ ->
			ToWokerList = action(TopoId,ToList),
			ToWokerList
	end.


getDataFromState(State)->
	TopoId = State#server_state.topo_id,
	Type = State#server_state.type,
	Name = State#server_state.type_name,
	{TopoId, Type, Name}.

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


ba(Path,Count)->
	ba(Path,Count-1,[]).
ba(_Path,-1,Result)->
	Result;
ba(Path,Index,Result)->	
	Path2 = sardine_utils:concatStrs([Path,"/",Index]),
	WorkerInfo = zk:get(Path2),
	ba(Path,Index-1,lists:append([Result,[WorkerInfo]])).	



action(TopoId, ToList)->
	action(TopoId, ToList, []).

action(TopoId, [], ResultList)->
	ResultList;
action(TopoId, [H|T] = ToList, ResultList)->
	Path = zk:genPath(TopoId, bolts, H),
	Count =   (zk:get(Path))#type_info.count,
	Result = ba(Path,Count),
	action(TopoId, T, lists:append([ResultList, Result])).
	
