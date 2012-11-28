%% @author simonlbw
%% @doc @todo Add description to ex_bolt.


-module(ex_bolt1).
-behaviour(sm_basic_bolt).

-include("../include/tuple.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([init/0, nextTuple/2]).

-record(state,{}).

init()->
	#state{}.

nextTuple(Tuple, State)
  when is_record(Tuple, tuple)->
	Content = Tuple#tuple.content,
	Content1 = string:concat(Content, "***"),
	State1 = State,
	{ok, #tuple{content=Content1}, State1}.
%% ====================================================================
%% Internal functions
%% ====================================================================


	