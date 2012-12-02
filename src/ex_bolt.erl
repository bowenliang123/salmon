%% @author simonlbw
%% @doc @todo Add description to ex_bolt.


-module(ex_bolt).
-behaviour(sm_basic_bolt).

-include("../include/tuple.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([init/0, nextTuple/2]).

-record(userData,{n}).

init()->
	#userData{n=0}.

nextTuple(Tuple, UserData)
  when is_record(Tuple, tuple)->
	Content = Tuple#tuple.content,
	N=UserData#userData.n,
	Content1 = sm_utils:concatStrs([Content,N+1]),
	State1 = UserData#userData{n=N + 1},
%%	error_logger:info_msg("~p!!!~n",[N]),
	{ok, #tuple{content=Content1}, State1}.
%% ====================================================================
%% Internal functions
%% ====================================================================


	