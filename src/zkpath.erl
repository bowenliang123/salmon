%% Author: simonlbw
%% Created: 2012-10-26
%% Description: TODO: Add description to zkpath
-module(zkpath).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([genPath/1, genPath/2, genPath/3,genPath/4]).

%%
%% API Functions
%%
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

