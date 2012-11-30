%% @author simonlbw
%% @doc @todo Add description to sm_spoutmsg.


-module(sm_spoutmsg).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/sm.hrl").
-include("../include/tuple.hrl").
-include("../include/sardine_config_interface.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([start_link/3]).

start_link(Tuple,From,SpoutConfig) when is_record(SpoutConfig, spoutConfig) ->
	gen_server:start_link(?MODULE, {Tuple,From,SpoutConfig}, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {tuple, from, spoutConfig, sentTupleIdsList}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init({Tuple, From, SpoutConfig}) when is_record(SpoutConfig, spoutConfig) ->
	error_logger:info_msg("Init ~p:~p~p~n",[?SPOUT_MSG_HANDLER,self(),SpoutConfig]),
	replyToFrom(Tuple),
	#spoutConfig{to=ToList,topoId=TopoId}=SpoutConfig,
	SentTupleIdsList = sendTupleToList(Tuple, ToList, TopoId),
	if SentTupleIdsList==[]->
			error_logger:info_msg("Terminating ~p:~p~n",[?SPOUT_MSG_HANDLER,self()]),
			spawn(supervisor,terminate_child,[?BOLT_MSG_SUP,self()]);
		true->ok
	end,
	{ok, #state{tuple=Tuple,from=From,spoutConfig=SpoutConfig,sentTupleIdsList=SentTupleIdsList}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({ack, TupleId}, State) ->
	SentTupleIdsList = State#state.sentTupleIdsList,
	SentTupleIdsList1 = lists:delete(TupleId, SentTupleIdsList),
	error_logger:info_msg("ack:~p~p~p~n",[TupleId,SentTupleIdsList,SentTupleIdsList1]),
	case SentTupleIdsList1==[] of
		true->
		   error_logger:info_msg("Terminating ~p:~p~n",[?SPOUT_MSG_HANDLER,self()]),
		   supervisor:terminate_child(?SPOUT_MSG_SUP,self());
	   false->
			  ok
	end,
	State1 = State#state{sentTupleIdsList=SentTupleIdsList1},
    {noreply, State1};
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

replyToFrom(Tuple)->
	ok.

sendTupleToList(Tuple, ToList, TopoId)->
	error_logger:info_msg("ToList~p~n",[ToList]),
	sendTupleToList(Tuple, ToList, TopoId, []).
sendTupleToList(Tuple, [H|T] = _ToList, TopoId, ResultList)->
	{TypeId,Grouping} = H,
	Path=sm_zk:genPath(TopoId, bolt, TypeId),
	{ok,IndexList}=sm_zk:ls(Path),
	case Grouping of
		shuffleGrouping->
			ResultTupleIdList = sendTupleToIndexList(Tuple, TopoId, TypeId, IndexList),
			sendTupleToList(Tuple, T, TopoId, lists:append([ResultList,ResultTupleIdList]))
	end;
sendTupleToList(_, [], _, ResultList)->
	error_logger:info_msg("SentTupleIdList~p~n",[ResultList]),
	ResultList.

sendTupleToIndexList(Tuple, TopoId, TypeId, IndexList)->
	sendTupleToIndexList(Tuple, TopoId, TypeId, IndexList,[]).
sendTupleToIndexList(Tuple, TopoId, TypeId, [H|T]=_IndexList, ResultList)->
	Path=sm_zk:genPath(TopoId, bolt, TypeId, H),
	{ok,R}=sm_zk:get(Path),
	case R of 
		{ActorName, Node} when is_atom(ActorName) and is_atom(ActorName)->
			random:seed(erlang:now()),
			TupleId = trunc(random:uniform()*1000000000),
			gen_server:cast({ActorName, Node},{nextTuple,Tuple#tuple{tupleId=TupleId},self()}),
			sendTupleToIndexList(Tuple, TopoId, TypeId, T, [TupleId|ResultList])
	end;
sendTupleToIndexList(_, _, _, [], ResultList)->
	ResultList.



	