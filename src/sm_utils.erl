%% Author: simonlbw
%% Created: 2012-10-24
%% Description: TODO: Add description to utils
-module(sm_utils).

%%
%% Include files
%%
-include("../include/zkdata_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([concatStrs/1]).
-export([isInList/2]).
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

isInList(Term, [H|T] = _TargetList) ->
	if Term == H->
		   true;
	   true->
		   isInList(Term, T)
	end;
isInList(_, [])->
	false.


genServerName(Type,TopoId,TypeId,Index) ->
	case Type of
		spout->
			ServerName = sm_utils:concatStrs(["spout",TopoId,TypeId,Index]);
		bolt->
			ServerName = sm_utils:concatStrs(["bolt",TopoId,TypeId,Index])
	end,
	list_to_atom(ServerName).



getModule(Type, TopoId, Name) ->
	Path = sm_zk:genPath(TopoId, Type, Name),
	SpoutTypeInfo = sm_zk:get(Path),
	Module = SpoutTypeInfo#type_info.module,
	Module.


getToWorkerList(SelfServerName) ->
	State = gen_server:call({global,SelfServerName}, getServerState),
	
	{TopoId, _Type, Name} = getDataFromState(State),
	ToList = sm_zk:get(sm_zk:genPath(TopoId, conns, Name)),
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
	Path2 = sm_utils:concatStrs([Path,"/",Index]),
	WorkerInfo = sm_zk:get(Path2),
	ba(Path,Index-1,lists:append([Result,[WorkerInfo]])).	



action(TopoId, ToList)->
	action(TopoId, ToList, []).

action(TopoId, [], ResultList)->
	ResultList;
action(TopoId, [H|T] = ToList, ResultList)->
	Path = sm_zk:genPath(TopoId, bolts, H),
	Count =    (sm_zk:get(Path))#type_info.count,
	Result = ba(Path,Count),
	action(TopoId, T, lists:append([ResultList, Result])).
	
