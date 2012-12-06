%% @author simonlbw
%% @doc @todo Add description to sm_zksitter.


-module(sm_zksitter).
-behaviour(gen_server).

-include("../include/sm.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).
-export([checkout/1]).

start_link(Path,SitterName)->
	gen_server:start_link({local,SitterName}, ?MODULE, [Path], []).

checkout(Path)->
	SitterName=genSitterName(Path),
	SitterPid = whereis(SitterName),
	if SitterPid==undefined->
		   supervisor:start_child(?ZK_SITTER_SUP, [Path, SitterName]);
	   true->
		   ok
	end,
	case gen_server:call(SitterName, checkout) of
		{ok, Data}->
			{ok, Data};
		{error, R}->
			{error, R}
	end.


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {zkData, connPid, path}).

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
init([Path]) ->
	{ok,ConnPid}=sm_zk:getConnection(),
	error_logger:info_msg("~p~n",[Path]),
	case ezk:get(ConnPid, Path, self(), changed) of
	{ok,{Data_bin, _}}->
			ZkData = {ok, binary_to_term(Data_bin)};
		{error, R}->
			ZkData = {error, R},
			spawn(supervisor,terminate_child,[?ZK_SITTER_SUP,self()])
%% 			supervisor:terminate_child(?ZK_SITTER_SUP,self())
	end,
	{ok, #state{zkData=ZkData, connPid=ConnPid, path=Path}}.


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
handle_call(checkout, From, State) ->
    {reply, State#state.zkData, State};
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
handle_info({changed, _}, State) ->
	#state{path=Path}=State,
	{ok,ConnPid}=sm_zk:getConnection(),
	case ezk:get(ConnPid, Path, self(), changed) of
		{ok,{Data_bin, _}}->
			ZkData = {ok, binary_to_term(Data_bin)};
		{error, R}->
			supervisor:terminate_child(?ZK_SITTER_SUP,self()),
			ZkData = {error, R}
	end,
    {noreply, State#state{zkData=ZkData,connPid=ConnPid}};
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
genSitterName(Path)->
	Tokens = string:tokens(Path,"/"),
	String = sm_utils:concatStrs(Tokens),
	list_to_atom("@"++String).

