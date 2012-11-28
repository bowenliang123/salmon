%% Author: simonlbw
%% Created: 2012-10-24
%% Description: TODO: Add description to utils
-module(sm_utils).

%%
%% Include files
%%
-include("../include/sardine_config_interface.hrl").

%%
%% Exported Functions
%%
-export([]).
-export([concatStrs/1]).
-export([isInList/2]).
-export([file_exist/1]).
-export([genServerName/4]).
-export([getModule/3]).

%%
%% API Functions
%%



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


genServerName(TopoId, Type,TypeId,Index) when is_atom(Type) ->
	case Type of
		spout->
			ServerName = sm_utils:concatStrs([TopoId,"_",TypeId,"_",Index]);
		bolt->
			ServerName = sm_utils:concatStrs([TopoId,"_",TypeId,"_",Index])
	end,
	list_to_atom(ServerName).



getModule(TopoId, Type, Name) when is_atom(Type) ->
	Path = sm_zk:genPath(TopoId, Type, Name),
	{ok, TypeInfo} = sm_zk:get(Path),
	case Type of
		spout->
			Module = TypeInfo#spoutConfig.module;
		bolt->
			Module = TypeInfo#boltConfig.module
	end,
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


