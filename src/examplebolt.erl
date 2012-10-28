%% Author: simonlbw
%% Created: 2012-10-26
%% Description: TODO: Add description to examplebolt
-module(examplebolt).

%%
%% Include files
%%
-include("../include/tuple_interface.hrl").

%%
%% Exported Functions
%%
-export([nextTuple/1]).

%%
%% API Functions
%%
nextTuple(#tupleMessage{message = Message } = Tuple) ->
	NextTuple = lists:append([Message, "!!!"]),
	#tupleMessage{message = NextTuple}.
	


%%
%% Local Functions
%%

