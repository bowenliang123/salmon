%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-11-22
%%% -------------------------------------------------------------------
-module(sm_bolt_actor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sm.hrl").
-include("../include/tuple.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {topoId, type, typeId, index, module, userData}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(TopoId, bolt, TypeId, Index) ->
	ActorName=sm_utils:genServerName(TopoId, bolt, TypeId, Index),
	gen_server:start_link({local,ActorName}, ?MODULE, [TopoId, bolt, TypeId, Index], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([TopoId, bolt, TypeId, Index]) ->
	ActorName=sm_utils:genServerName(TopoId, bolt, TypeId, Index),
	error_logger:info_msg("Initial ~p:~p~n", [?BOLT_ACTOR,ActorName]),
	Module = sm_utils:getModule(TopoId, bolt, TypeId),
    {ok, #state{topoId=TopoId, type=bolt, typeId=TypeId, index=Index, module=Module}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({nextTuple, Tuple}, State) ->
	#state{module=Module,userData=UserData}=State,
	{ok, Tuple1, UserData1}=Module:nextTuple(Tuple,UserData),
	error_logger:info_msg("TupleDealed!!:~p~n",[Tuple1]),
	State1 = State#state{userData=UserData1},
    {noreply, State1};
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

