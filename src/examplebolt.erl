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
-export([nextTuple/0]).

%%
%% API Functions
%%
nextTuple() ->
	timer:sleep(3000),
	Message = "i am examplebolt!",
	#tupleMessage{message = Message}.
	


%%
%% Local Functions
%%

