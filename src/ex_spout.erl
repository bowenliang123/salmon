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

-record(userData,{n}).

init()->
	#userData{n=0}.

nextTuple(UserData) ->
	Content1 = "From Spout",
	N=UserData#userData.n,
	{ok, #tuple{content=Content1}, UserData#userData{n=N+1}}.
%% ====================================================================
%% Internal functions
%% ====================================================================
