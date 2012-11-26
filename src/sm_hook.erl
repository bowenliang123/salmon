%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-11-23
%%% -------------------------------------------------------------------
-module(sm_hook).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sm.hrl").
-include("../include/sardine_config_interface.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).
-export([start_link/0]).
-export([startFishing/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-define(FOUND, found).
-define(NOT_FOUND, not_found).

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
	error_logger:info_msg("Initial ~p~n",[?MODULE]),
	gen_server:start_link({local,?HOOK}, ?MODULE, [], []).

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
init([]) ->
	error_logger:info_msg("Initial ~p~n", [?MODULE]),
	{ok, DEFAULT_HOOK_INTERVAL} = application:get_env(default_hook_interval),
	spawn(?MODULE,startFishing,[DEFAULT_HOOK_INTERVAL]),
    {ok, #state{}}.

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
startFishing(Interval)->
	timer:sleep(Interval),
	ReadyToposIdList = getReadyToposIdList(),
	case getFirstEmptyActor(ReadyToposIdList) of
		?NOT_FOUND->
			ok;
		{?FOUND, {TopoId, Type, TypeId, Index}} = Response->
			error_logger:info_msg("WE GOT ~p~n",[Response]),
			Path=sm_zk:genPath(TopoId, Type, TypeId, Index),
			sm_zk:set(Path, "hi"),
			ServerName=sm_utils:genServerName(Type, TopoId, TypeId, Index),
			case Type of
				bolt->
					error_logger:info_msg("Bolt!~p~n",[ServerName]),
					supervisor:start_child(?BOLTS_SUP, [ServerName]);
				spout->
					error_logger:info_msg("Spout!~p~n",[ServerName]),
					supervisor:start_child(?SPOUTS_SUP, [ServerName])
			end
	end,
	spawn(?MODULE,startFishing,[Interval]),
	ok.

getReadyToposIdList()->
	{ok,ToposIdList} = sm_zk:ls(sm_zk:rootPath()),
	ResultList = pickReadyTopos(ToposIdList),
	error_logger:info_msg("ResultList:~p~n",[ResultList]),
	ResultList.
	

pickReadyTopos(ToposIdList)->
	pickReadyTopos(ToposIdList,[]).
pickReadyTopos([H|T] = ToposIdList, ResultToposIdList)->
	case isReadyStatus(H) of
		false->
			pickReadyTopos(T,ResultToposIdList);
		true->
			pickReadyTopos(T,[H|ResultToposIdList])
	end;
pickReadyTopos([], ResultToposIdList)->
	ResultToposIdList.


isReadyStatus(TopoId)->
	Path = sm_zk:genPath(TopoId),
	try
		{ok, TopoConfig}=sm_zk:get(Path),
		Status =  TopoConfig#topoConfig.status,
		error_logger:info_msg("~p-~p~n",[TopoId,Status]),
		case Status of
			ready->
				true;
			_->
				false
		end
	catch
		error:_ ->
			false
	end.
	
getFirstEmptyActor([H|T] = _ToposIdList)->
	case findFirstEmptyActor(H,spout) of
		{?FOUND, EmptyActorData} ->
			{?FOUND, EmptyActorData};
		?NOT_FOUND ->
			case findFirstEmptyActor(H,bolt) of
				{?FOUND, EmptyActorData} ->
					{?FOUND, EmptyActorData};
				?NOT_FOUND ->
					getFirstEmptyActor(T)
			end				
	end;
getFirstEmptyActor([])->
	?NOT_FOUND.
	
findFirstEmptyActor(TopoId, Type)->
	Path = sm_zk:genPath(TopoId, Type),
	try
		{ok, TypeIdList} = sm_zk:ls(Path),
		case findFirstEmptyActor(TopoId, Type,TypeIdList) of
			?NOT_FOUND ->
				?NOT_FOUND;
			{?FOUND, Data} ->
				{?FOUND, Data}	
		end
	catch
		error:_ -> ?NOT_FOUND
	end.

findFirstEmptyActor(TopoId, Type, [H|T]  =_TypeIdList) ->
	Path = sm_zk:genPath(TopoId, Type, H),
	try
		{ok, ActorIndexList} = sm_zk:ls(Path),
		case findFirstEmptyActor(TopoId, Type, H, ActorIndexList) of
			?NOT_FOUND ->
				findFirstEmptyActor(TopoId, Type,T);
			{?FOUND, Data}->
				{?FOUND, Data}
		end	
	catch
		error:_ -> findFirstEmptyActor(TopoId, Type,T)
	end;
findFirstEmptyActor(_, _, []) ->
	?NOT_FOUND.
								  
findFirstEmptyActor(TopoId, Type, TypeId, [H|T]=_ActorIndexList) ->
	Path = sm_zk:genPath(TopoId, Type, TypeId, H),
	try
		{ok,Data} = sm_zk:get(Path),
		case Data of
			null->
				{?FOUND, {TopoId, Type, TypeId, H}};
			_->
				findFirstEmptyActor(TopoId, Type, TypeId, T)
		end
	catch
		error:_ -> 
			findFirstEmptyActor(TopoId, Type, TypeId, T)
	end;
findFirstEmptyActor(_, _, _, []) ->
	?NOT_FOUND.