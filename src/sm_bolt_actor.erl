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
-include("../include/sardine_config_interface.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config, index, userData}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(BoltConfig,Index) when is_record(BoltConfig, boltConfig)->
	#boltConfig{topoId=TopoId, id=TypeId} = BoltConfig,
	ActorName=sm_utils:genServerName(TopoId, bolt, TypeId, Index),
	gen_server:start_link({local,ActorName}, ?MODULE, {BoltConfig,Index}, []).

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
init({BoltConfig,Index}) when is_record(BoltConfig, boltConfig)->
	#boltConfig{topoId=TopoId, id=TypeId} = BoltConfig,
	ActorName=sm_utils:genServerName(TopoId, bolt, TypeId, Index),
	error_logger:info_msg("Initial ~p:~p~n", [?BOLT_ACTOR,ActorName]),
	Module = sm_utils:getModule(TopoId, bolt, TypeId),
	UserData=Module:init(),
    {ok, #state{config=BoltConfig, index=Index, userData=UserData}}.

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
handle_call(check, From, State) ->
    error_logger:info_msg("~p~n",[State]),
    {reply, ok, State};
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
handle_cast({nextTuple, Tuple, From}, State) when is_pid(From)->
	gen_server:cast(From,{ack,Tuple#tuple.tupleId}),
	#state{config=BoltConfig,userData=UserData}=State,
	#boltConfig{module=Module,to=To}=BoltConfig,
	{ok, Tuple1, UserData1}=Module:nextTuple(Tuple,UserData),
	case To of
		[]->ok;
		[_H|_]->
			supervisor:start_child(?BOLT_MSG_SUP,[Tuple1,From,BoltConfig])
	end,
    {noreply, State#state{userData=UserData1}}.
%% handle_cast(Msg, State) ->
%%     {noreply, State}.

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

