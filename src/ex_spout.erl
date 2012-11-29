%% @author simonlbw
%% @doc @todo Add description to ex_bolt.


-module(ex_spout).
-behaviour(sm_basic_spout).

-include("../include/tuple.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([init/0, nextTuple/2]).

-record(userData,{}).

init()->
	#userData{}.

nextTuple(Tuple, UserData)
  when is_record(Tuple, tuple)->
	Content = Tuple#tuple.content,
	Content1 = string:concat(Content, "!!!"),
	State1 = UserData,
	{ok, #tuple{content=Content1}, State1}.
%% ====================================================================
%% Internal functions
%% ====================================================================


	