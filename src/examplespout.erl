%% Author: simonlbw
%% Created: 2012-10-25
%% Description: TODO: Add description to examplespout
-module(examplespout).

%%
%% Include files
%%
-include("../include/tuple_interface.hrl").

%%
%% Exported Functions
%%
-export([nextTuple/0]).

%%
%% API Functions
%%
nextTuple() ->
	timer:sleep(3000),
	Message = "i am examplespout!",
	#tupleMessage{message = Message}.
	


%%
%% Local Functions
%%

