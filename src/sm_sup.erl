%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-11-22
%%% -------------------------------------------------------------------
-module(sm_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sm.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	error_logger:info_msg("Initial ~p~n", [?SM_SUP]),
	Hook_sup = {?HOOK_SUP,{?HOOK_SUP,start_link,[]},
	          permanent,infinity,supervisor,[]},
    Spouts_sup = {?SPOUTS_SUP,{?SPOUTS_SUP,start_link,[]},
	          permanent,infinity,supervisor,[]},
	Bolts_sup = {?BOLTS_SUP,{?BOLTS_SUP,start_link,[]},
	          permanent,infinity,supervisor,[]},
	
	SpoutMsg_sup = {?SPOUT_MSG_SUP,{?SPOUT_MSG_SUP,start_link,[]},
	          permanent,infinity,supervisor,[]},
	BoltMsg_sup = {?BOLT_MSG_SUP,{?BOLT_MSG_SUP,start_link,[]},
	          permanent,infinity,supervisor,[]},
	
    {ok,{{one_for_all,1,1}, [SpoutMsg_sup, BoltMsg_sup,Spouts_sup, Bolts_sup,Hook_sup]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

