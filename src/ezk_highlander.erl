
%% -------------------------------------------------------------------
%%
%% ezk_highlander: A behaviour that exactly one instance of a module per given path runs.
%%
%% Copyright (c) 2011 Marco Grebe. All Rights Reserved.
%% Copyright (c) 2011 global infinipool GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(ezk_highlander).

-include_lib("../include/ezk.hrl").
 
-behaviour(gen_server).


-type zk_conn() :: pid().
-type zk_nodename() :: string().

-record(high_state, {is_active = false,
		     ident,
		     my_path,
		     module, 
		     parameters,
		     wait_for_active = [],
		     module_state,
		     connection_pid
		    }).

-export([start/4, start_link/4, failover/2,wait_for/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([behaviour_info/1]).
-export([is_active/1]).


behaviour_info(callbacks) ->
    [{terminate,2}, {init,2}, {motto,1}, {no_quickening,2}].

%% The startfunction.
start(ConnectionPId, Module, Parameters, NodeList) ->
    gen_server:start( ?MODULE, [ConnectionPId, Module, Parameters, NodeList], []).

start_link(ConnectionPId, Module, Parameters, NodeList) ->
    gen_server:start_link(?MODULE, [ConnectionPId, Module, Parameters, NodeList], []).


%% wait for a highlander to appear. stop with error(timeout, Nodename) in case of timeout
%% @param NodeName path to the Highlander's 
-spec wait_for(zk_conn(), zk_nodename(), pos_integer()) ->
		      ok.
wait_for(ConnectionPid, Nodename, Timeout) ->
    Timer = erlang:start_timer(Timeout, self(), timeout),
    ok = wait_for_get(ConnectionPid, Nodename, Timer),
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end,
    ok.

wait_for_get(ConnectionPid, Nodename, Timer) ->
    receive {timeout, Timer, _} -> 
	    erlang:error(timeout, Nodename)
    after 0 -> ok
    end,
    case ezk:get(ConnectionPid, Nodename) of
	{ok, _} ->
	    ok;
	_ ->
	    timer:sleep(50),
	    wait_for_get(ConnectionPid, Nodename, Timer)
    end.


%% The init function trys every given path once and ensures that all
%% needed ZK Nodes are there. After trying once without success it goes 
%% into normal genserver state and waits for messages ( = changes in the important nodes)
%% Return type is a record of type high_state, which is the State format in this Server.
init([ConnectionPId, Module, Parameters, NodeList]) ->
    process_flag(trap_exit, true),    
    %% ensure the needed Paths
    ?LOG(1,"Highlander: Init: got parameters module, Para, NodeList: ~w, ~w, ~w",
	 [Module, Parameters, NodeList]), 
    lists:map(fun(Path) ->
		      Father = get_father(Path),
		      ?LOG(1,"Highlander: ensuring Path ~s",[Father]),
		      ezk:ensure_path(ConnectionPId, Father) ,
		      ?LOG(1,"Highlander: Path ensured: ~s",[Father])
	      end, NodeList),
    %% set initial state
    State = #high_state{ident = false, my_path = "", module = Module, 
			parameters = Parameters,  module_state = false,
		        connection_pid = ConnectionPId},
    %% first try to get highlander at every node
    ?LOG(1,"Highlander: Start a first try on all paths"),
    case try_first(Module, ConnectionPId, NodeList) of
	no_luck ->
	    ?LOG(1, "Highlander: No luck in first round"),
	    {ok, State};
	{ok, Path} ->
	    ?LOG(1, "Highlander:  Init: ~w trying to start its child", [self()]),
	    {ok, NewState} = start_init(Module, Parameters, 
					State#high_state{my_path = Path}),
	    ?LOG(1, "Highlander:  Init: ~w Child started", [self()]),
	    {ok, NewState#high_state{is_active = true}}
    end.

%% Gives true or false, depending on if the instance has become a highlander.
is_active(PId) ->
    gen_server:call(PId, isactive).

%% Triggers a stopping of the highlander.
failover(PId, Reason) ->
    gen_server:call(PId, {failover, Reason}).

%% Used if the server dies (also in case of a failover)
%% calls terminate/2 of the child. After 2 seconds kills the child
%% and the itself.
terminate(Reason, State) ->
    ?LOG(1, "Highlander: Failover  of ~w",[self()]),
    Module = State#high_state.module,
    spawn(fun() -> Module:terminate(State#high_state.module_state, Reason) end),
    %% if terminate already deleted the node it must not be deleted again.
    ConnectionPId = State#high_state.connection_pid,
    Path  = State#high_state.my_path, 
    Motto = Module:motto(Path),
    case ezk:get(ConnectionPId, Path) of
	{ok, {Motto, _I}} ->
	    ezk:delete(ConnectionPId, Path);
	_Else -> 
	    Motto = Module:motto(Path)	    
    end,
    ok.
%    timer:sleep(2000).

%% Called by the failover function
handle_call({failover, Reason}, _From, State) ->    
    {stop,  {shutdown, Reason}, ok, State};
%% Called by the is_active function.
handle_call(isactive, _From, State) ->
    {reply, State#high_state.is_active, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


%% If a watch is triggered this Message comes to the Highlander. 
%% Path is the Path of the Node to make, not the one of the Father. 
handle_info({{nodechanged, _Path}, _I}, #high_state{is_active=true} = State) ->
    %% IF the highlander is already active  we ignore this messages. 
    %% This way we don't have to determine how many unused watches we left
    %% triggered and are save from miscalculations happening while doing so.
    ?LOG(1,"~w is already a highlander", [self()]),
    {noreply, State};
handle_info({{nodechanged, Path}, _I}, #high_state{is_active=false} = State) ->
    ?LOG(1," Highlander: nodechangenotify: ~w got one for ~s",[self(), Path]),
    %% if not active we try to get. 
    ConnectionPId = State#high_state.connection_pid,
    Module = State#high_state.module,
    case try_to_get(Module, ConnectionPId, Path) of
	%% If successful we can modify the State and trigger the init
	{ok, Path} ->
	    ?LOG(1,"~w was lucky in retry", [self()]),
	    Parameters = State#high_state.parameters,
	    {ok, NewState} = start_init(Module, Parameters, 
					State#high_state{my_path = Path}),
	    {noreply, NewState#high_state{is_active = true}};
	%% If we do not win the race we go back to start.
	{error, _I1} ->
	    did_not_get_highlander(Module, ConnectionPId, Path),
	    ?LOG(1,"~w was not lucky in retry", [self()]),
	    {noreply, State}
    end;
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.



handle_cast(_A, State) ->
    {noreply, State}.


%% Starts the Highlandchild.
%% The run function gets the information 
%% a) who his father is, b) the highlander of which path it is
%% and c) the parameters provided when start was called.
start_init(Module, Parameters, State) ->
    ?LOG(1, "Highlander : start_child: Function called."),
    Path   = State#high_state.my_path,
    ?LOG(1, "Highlander : start_child: Starting Child function run"),
    {ok, ModuleState} = Module:init(Path, Parameters),
    ?LOG(1, "Highlander : start_child: run is running"),
    {ok, State#high_state{module_state = ModuleState}}.

%% Makes a first try to get highlander on every path from the list.
%% After this every fathernode  has a childwatch.
try_first(_Module, _ConnectionPId, []) ->
    ?LOG(1, "Highlander: First Try: ~w tried all without luck",[self()]),
    no_luck;
try_first(Module, ConnectionPId, [Node | NodeList]) ->
    case (try_to_get(Module, ConnectionPId, Node)) of
	{ok, Path} ->
	    ?LOG(1, "Highlander: First Try: ~w got lucky",[self()]),
	    {ok, Path};
	{error, _I}  ->
	    did_not_get_highlander(Module, ConnectionPId, Node),
	    try_first(Module, ConnectionPId, NodeList)
    end.  


did_not_get_highlander(Module, ConnectionPId, Path) ->
    case ezk:get(ConnectionPId, Path) of
	{ok, {WinnersMotto, _Stuff}} ->
	    Module:no_quickening(Path, WinnersMotto);
	_Else ->
	    ok
    end.
    

%% Trys to create a the in Path specified node with data Ident.
%% Also sets a childwatch to its father with message {nodechanged, Path}.
try_to_get(Module, ConnectionPId, Path) ->
    ?LOG(1, "Highlander: Getting the Motto from Module ~s for Path ~s",[Module, Path ]),    
    Motto = Module:motto(Path),
    Father = get_father(Path),
    ?LOG(1, "Highlander: Setting watch to Father: ~w. Motto is ~s",[Father, Motto]),
    ezk:ls(ConnectionPId, Father, self(), {nodechanged, Path}),
    ?LOG(1, "Highlander: Watch set by ~w",[self()]),
    ezk:create(ConnectionPId, Path, Motto, e).

%% gets the father's address by looking at the child's
get_father(Path) ->
    FullPath = string:tokens(Path, "/"),
    get_father_from_list(FullPath).
get_father_from_list([_H]) ->
    "";
get_father_from_list([ H | T ]) ->
    "/" ++ H ++ get_father_from_list(T).

%% the needed swap function. 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
