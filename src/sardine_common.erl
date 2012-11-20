%% Author: Administrator
%% Created: 2012-11-17
%% Description: TODO: Add description to sardine_common
-module(sardine_common).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-export([isInList/2]).

%%
%% API Functions
%%
isInList(Term, [H|T] = _TargetList) ->
	if Term == H->
		   true;
	   true->
		   isInList(Term, T)
	end;
isInList(_, [])->
	false.

%%
%% Local Functions
%%

