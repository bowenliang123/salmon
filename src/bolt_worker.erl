%%% -------------------------------------------------------------------
%%% Author  : simonlbw
%%% Description :
%%%
%%% Created : 2012-10-27
%%% -------------------------------------------------------------------
-module(bolt_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/zkdata_interface.hrl").
%% --------------------------------------------------------------------
%% External exportss
-export([]).
-export([start_link/3]).
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(TopoId, Spout, Index) ->
	SpoutServerName = utils:genServerName(bolt, TopoId, Spout, Index),
	gen_server:start_link({global,SpoutServerName}, bolt_worker, [TopoId, Spout, Index], []),
	startrun(TopoId,Spout,Index).

startrun(TopoId,Spout,Index) ->
	SpoutServerName = utils:genServerName(bolt, TopoId, Spout, Index),
	gen_server:cast({global,SpoutServerName}, startrun),
	ok.

execute(Module, SelfServerName)->
	io:format("it is run~n"),
	
	SpoutTuple = Module:nextTuple(),
	io:format("~p~n",[SpoutTuple]),
	ToServerList = getToWorkerList(SelfServerName),
	emitTuples(SpoutTuple,ToServerList),
	execute(Module, SelfServerName).

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
init([TopoId, SpoutName, Index]) ->
	io:format("it is bolt_server init~n"),
	SelfServerName = utils:genServerName(bolt, TopoId, SpoutName, Index),
	SpoutModule = utils:getModule(bolt, TopoId, SpoutName),
	io:format("Module:~p~n", [SpoutModule]),
	
	WorkerPath = zkpath:genPath(TopoId, bolt, SpoutName, Index),
	utils:zkset(WorkerPath, #worker_info{self_name = SelfServerName, node_name = node()}),
	
	
	OriginalState = #server_state{self_name = SelfServerName,
								  topo_id = TopoId,
								  type_name = SpoutName,
								  index = Index,
								  module = SpoutModule},
	io:format("OriginalState:~p~n", [OriginalState]),
	
    {ok, OriginalState}.

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
handle_call(getServerState, From, State) ->
    {reply, State, State};
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

handle_cast(startrun, State) ->
	Module = State#server_state.module,
	SelfServerName = State#server_state.self_name,
	PID = spawn(spout_worker,execute,[Module, SelfServerName]),
 {noreply, State};
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
getToWorkerList(SelfServerName) ->
	State = gen_server:call({global,SelfServerName}, getServerState),
	{TopoId, Type, Name} = getDataFromState(State),
	ToList = utils:zkget(zkpath:genPath(TopoId, conns, Name)),
	ToWokerList = action(TopoId,ToList),
	io:format("ToWL:~p~n", [ToWokerList]),
	ToWokerList.

ba(Path,-1,Result)->
	Result;
ba(Path,Index,Result)->	
	Path2 = utils:concatStrs([Path,"/",Index]),
	WorkerInfo = utils:zkget(Path2),
	ba(Path,Index-1,lists:append([Result,[WorkerInfo]])).	
ba(Path,Count)->
	ba(Path,Count-1,[]).
	

action(TopoId, [], ResultList)->
	ResultList;
action(TopoId, [H|T] = ToList, ResultList)->
	Path = zkpath:genPath(TopoId, bolts, H),	
	Count = (utils:zkget(Path))#type_info.count,
	io:format("H:~p~p~p~n", [H,Path,Count]),
	Result = ba(Path,Count),
	action(TopoId, T, lists:append([ResultList, Result])).
action(TopoId, ToList)->
	action(TopoId, ToList, []).
	

getDataFromState(State)->
	TopoId = State#server_state.topo_id,
	Type = State#server_state.type,
	Name = State#server_state.type_name,
	{TopoId, Type, Name}.

emitTuples(SpoutTuple,ToServerList) ->
	io:format("emitTuples"),
	ok.