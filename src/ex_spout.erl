%% @author simonlbw
%% @doc @todo Add description to ex_bolt.


-module(ex_spout).
-behaviour(sm_basic_spout).

-include("../include/tuple.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([init/0, nextTuple/1]).

-record(userData,{}).

init()->
	#userData{}.

nextTuple(UserData) ->
	Content1 = "From Spout",
	State1 = UserData,
	{ok, #tuple{content=Content1}, State1}.
%% ====================================================================
%% Internal functions
%% ====================================================================
