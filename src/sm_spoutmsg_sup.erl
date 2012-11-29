%% @author simonlbw
%% @doc @todo Add description to sm_spoutmsg_sup.


-module(sm_spoutmsg_sup).
-behaviour(supervisor).

-include("../include/sm.hrl").

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([start_link/0]).

start_link()->
	supervisor:start_link({local,?SPOUT_MSG_SUP}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    error_logger:info_msg("Initial ~p~n", [?SPOUT_MSG_SUP]),
  	SpoutMsgHandler = {?SPOUT_MSG_HANDLER,{?SPOUT_MSG_HANDLER,start_link,[]},
	                temporary,1000,worker,[?SPOUT_MSG_HANDLER]},
    {ok,{{simple_one_for_one,1,1}, [SpoutMsgHandler]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


