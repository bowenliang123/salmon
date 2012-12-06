%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-11-22
%%% -------------------------------------------------------------------
-module(sm_spout_actor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sm.hrl").
-include("../include/tuple.hrl").
-include("../include/sardine_config_interface.hrl").
-include("../include/sm_topostatus.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).
-export([start_link/2]).
-export([loopSpring/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config, userData}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(SpoutConfig,Index) when is_record(SpoutConfig, spoutConfig) ->
	#spoutConfig{topoId=TopoId, id=TypeId} = SpoutConfig,
	ActorName=sm_utils:genServerName(TopoId, spout, TypeId, Index),
	gen_server:start_link({local,ActorName}, ?MODULE, {SpoutConfig,Index}, []).

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
init({SpoutConfig,Index}) when is_record(SpoutConfig, spoutConfig)->
	#spoutConfig{topoId=TopoId, id=TypeId} = SpoutConfig,
	ActorName=sm_utils:genServerName(TopoId, spout, TypeId, Index),
	error_logger:info_msg("Initial ~p:~p~n", [?SPOUT_ACTOR,ActorName]),
	Module = sm_utils:getModule(TopoId, spout, TypeId),
	UserData=Module:init(),
	spawn_link(?MODULE, loopSpring, [SpoutConfig,Index]),
    {ok, #state{config=SpoutConfig,userData=UserData}}.

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
handle_call({check, FromPid},_,State)->
	{reply,State,State};
handle_call({nextTuple, FromPId}, _From, State) when is_pid(FromPId)->
	#state{config=SpoutConfig,userData=UserData}=State,
	#spoutConfig{module=Module}=SpoutConfig,
	{ok, Tuple1, UserData1}=Module:nextTuple(UserData),
	supervisor:start_child(?SPOUT_MSG_SUP,[Tuple1,FromPId,SpoutConfig]),
    {reply, ok, State#state{userData=UserData1}}.
%% handle_call(Request, From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

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
loopSpring(SpoutConfig,Index)->
	loopSpring(SpoutConfig,Index,0).
loopSpring(SpoutConfig,Index,N) when is_record(SpoutConfig, spoutConfig)->
	#spoutConfig{topoId=TopoId, id=TypeId} = SpoutConfig,
	case sm_zk:getTopoStatus(TopoId) of
		?TOPO_STATUS_READY->
			ActorName=sm_utils:genServerName(TopoId, spout, TypeId, Index),
			gen_server:call(ActorName,{nextTuple, self()}),
%%			error_logger:info_msg("~p~n",[N]),
%% 			if N rem 2000==0 ->
%% 				   error_logger:info_msg("loopSpring:~p~n",[N]);
%% 			   true->
%% 				   ok
%% 			end,
			loopSpring(SpoutConfig,Index,N+1);
		_->
			timer:sleep(2000),
			loopSpring(SpoutConfig,Index,0)
	end.

	
	
