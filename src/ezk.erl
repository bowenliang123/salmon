
%% -------------------------------------------------------------------
%%
%% ezk: The Interface Module. No real functions (but help/0).
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

-module(ezk).
-include_lib("../include/ezk.hrl").

%functions creating and deleting zkNodes
-export([  create/3,   create/4,   create/5,   delete/2]).
-export([n_create/5, n_create/6, n_create/7, n_delete/4]).
%functions dealing with node informations
-export([  set/3,   get/2,   ls/2,   ls2/2,   set_acl/3,   get_acl/2]).
-export([n_set/5, n_get/4, n_ls/4, n_ls2/4, n_set_acl/5, n_get_acl/4]).
%functions dealing with watches
-export([ls/4, get/4, ls2/4]).
%macros
-export([delete_all/2, ensure_path/2]).
%infos
-export([info_get_iterations/1,  help/0]).
%Stop commands (forcing Client to choose a new random Server from List)
-export([die/1, die/2, auth/3]).

-export([start_connection/0, start_connection/1, end_connection/2]).
-export([add_monitors/2, get_connections/0]).
-export([exists/2, exists/4]).

-type ezk_err()          :: inval_acl | dir_exists | no_rights | no_dir | 
			    childs_or_forbidden.
-type ezk_path()         :: string().
-type ezk_conpid()       :: pid().
-type ezk_data()         :: binary().
-type ezk_ctype()        :: e | es | s | se.
-type ezk_acl_perms()    :: [ezk_acl_perm()].
-type ezk_acl_perm()     :: r | w | c | d | a.
-type ezk_acl_scheme()   :: string().
-type ezk_acl_id()       :: string().
-type ezk_acl()          :: {ezk_acl_perms(), ezk_acl_scheme(), ezk_acl_id()}.
-type ezk_acls()         :: [ezk_acl()].
-type ezk_getdata()      :: {}.
-type ezk_watchowner()   :: pid().
-type ezk_watchmessage() :: term().
-type ezk_ls2data()      :: {children, [ezk_path()]} | {getdata, ezk_getdata()}.
-type ezk_server()       :: {}.
-type ezk_monitor()      :: pid().
-type ezk_authreply()    :: {ok, authed} | {error, auth_failed} | 
			    {error, unknown, binary()} | {error,  auth_in_progress}.

-spec create/3 :: (ezk_conpid(), ezk_path(), ezk_data()) ->
			  {ok, ezk_path()} | {error, ezk_err()}.
-spec create/4 :: (ezk_conpid(), ezk_path(), ezk_data(), ezk_ctype()) ->
			  {ok, ezk_path()} | {error, ezk_err()}.
-spec create/5 :: (ezk_conpid(), ezk_path(), ezk_data(), ezk_ctype(), ezk_acls()) ->
			  {ok, ezk_path()} | {error, ezk_err()}.
-spec ensure_path/2 :: (ezk_conpid(), ezk_path()) ->
			       {ok, ezk_path()} | {error, ezk_err()}.
-spec delete/2 :: (ezk_conpid(), ezk_path()) ->
			  {ok, ezk_path()} | {error, ezk_err()}.
-spec delete_all/2 ::  (ezk_conpid(), ezk_path()) ->
			       {ok, ezk_path()} | {error, ezk_err()}.
-spec exists/2 :: (ezk_conpid(), ezk_path()) ->
			  {ok, ezk_getdata()} | {error, ezk_err()}.
-spec exists/4 :: (ezk_conpid(), ezk_path(), ezk_watchowner(), ezk_watchmessage()) ->
			  {ok, ezk_getdata()} | {error, ezk_err()}.
-spec ls/2     :: (ezk_conpid(), ezk_path()) ->
			  {ok, [ezk_path()]} | {error, ezk_err()}.
-spec ls/4     :: (ezk_conpid(), ezk_path(), ezk_watchowner(), ezk_watchmessage()) ->
			  {ok, [ezk_path()]} | {error, ezk_err()}.
-spec ls2/2    :: (ezk_conpid(), ezk_path()) ->
			  {ok, [ezk_ls2data()]} | {error, ezk_err()}.
-spec ls2/4    :: (ezk_conpid(), ezk_path(), ezk_watchowner(), ezk_watchmessage()) ->
			  {ok, [ezk_ls2data()]} | {error, ezk_err()}.
-spec get/2    :: (ezk_conpid(), ezk_path()) ->
			  {ok, {ezk_data(), ezk_getdata()}} | {error, ezk_err()}.
-spec get/4    :: (ezk_conpid(), ezk_path(), ezk_watchowner(), ezk_watchmessage()) ->
			  {ok, {ezk_data(), ezk_getdata()}} | {error, ezk_err()}.
-spec set/3    :: (ezk_conpid(), ezk_path(), ezk_data()) ->
			  {ok, ezk_getdata()} | {error, ezk_err()}.
-spec set_acl/3:: (ezk_conpid(), ezk_path(), ezk_acls()) ->
			  {ok, ezk_getdata()}.
-spec get_acl/2:: (ezk_conpid, ezk_path()) ->
			  {ok, {ezk_acls(), ezk_getdata()}}.

-spec start_connection/0 :: () -> {ok, ezk_conpid()} | {error, no_server_reached}.
-spec start_connection/1 :: (ezk_server()) ->
					      {ok, ezk_conpid()} |
				    {error, no_server_reached}.
-spec end_connection/2   :: (ezk_conpid(), string()) -> ok | {error, no_connection}.
-spec add_monitors/2     :: (ezk_conpid(), [pid()])  -> ok.
-spec get_connections/0     :: () -> [{ezk_conpid(), [ezk_monitor()]}].
-spec info_get_iterations/1 :: (ezk_conpid()) -> integer().
-spec auth/3                :: (ezk_conpid(), ezk_acl_scheme(), ezk_acl_id()) ->
				       ezk_authreply().
				    
			  
			     

help() ->
    io:format("--------------------------------------------------------------~n"),
    io:format("| The Commands this Client knows about:                      |~n"),
    io:format("|------------------------------------------------------------|~n"),
    io:format("| ezk:create/3     : ConPId,  Path, Data                     |~n"),
    io:format("| ezk:create/4     : ConPId,  Path, Data, Typ                |~n"),
    io:format("| ezk:create/5     : ConPId,  Path, Data, Typ, [Acl]         |~n"),
    io:format("| ezk:delete/2     : ConPId,  Path                           |~n"),
    io:format("| ezk:delete_all/2 : ConPId,  Path                           |~n"),
    io:format("| ezk:exists/2     : ConPId,  Path                           |~n"),
    io:format("| ezk:exists/4     : ConPId,  Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:get/2        : ConPId,  Path                           |~n"),
    io:format("| ezk:get/4        : ConPId,  Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:get_acl/2    : ConPId,  Path                           |~n"),
    io:format("| ezk:set/3        : ConPId,  Path, Data                     |~n"),
    io:format("| ezk:set_acl/3    : ConPId,  Path, [Acl]                    |~n"),
    io:format("| ezk:ls/2         : ConPId,  Path                           |~n"),
    io:format("| ezk:ls/4         : ConPId,  Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:ls2/2        : ConPId,  Path                           |~n"),
    io:format("| ezk:ls2/4        : ConPId,  Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:info_get_iterations/1  : ConPId                        |~n"),
    io:format("| ezk:start_connection/0                                     |~n"),
    io:format("| ezk:start_connection/1     : Servers                       |~n"),
    io:format("| ezk:end_connection/2       : ConPId, Reason                |~n"),
    io:format("| ezk:addMonitors/2          : ConPId, MonitorPIds           |~n"),
    io:format("| ezk:getConnections/0                                       |~n"),
    io:format("|------------------------------------------------------------|~n"),
    io:format("| In Progress:                                               |~n"),
    io:format("| ezk:auth/3       : ConPId,  Scheme, Id                     |~n"),
    io:format("| --> Dangerous function. Fail auths get the                 |~n"),
    io:format("|     zkServer to close the Session!                         |~n"),
    io:format("|      ---                                ---                |~n"),
    io:format("| Nonblocking Calls:                                         |~n"),
    io:format("|     The last 2 Parameters are PId of the receiver          |~n"),
    io:format("|       and a Tag. The Answermessage is {Tag,Reply}          |~n"),
    io:format("| n_create/4, n_create/5, n_create/6,   n_delete/3           |~n"),
    io:format("| n_set/4,    n_get/3,    n_set_acl/4,  n_get_acl/3          |~n"),
    io:format("| n_ls/3,     n_ls2/3                                        |~n"),
    io:format("|------------------------------------------------------------|~n"),
    io:format("| Datatypes:                                                 |~n"),
    io:format("| Acl = {Scheme,Id, [Permission]}                            |~n"),
    io:format("| Path = Scheme = Id = Reason = String                       |~n"),  
    io:format("| Permission = r | w | c | d | a                             |~n"),
    io:format("| WatchOwner, ConPId = PId           WatchMessage = String   |~n"),
    io:format("| Data    = All Things              Typ = e | s | es         |~n"),
    io:format("| Server  = {IP, Port, Timeout(ms), Heartbeattime(ms)        |~n"),
    io:format("| Servers = [Server]                                         |~n"),
    io:format("|------------------------------------------------------------|~n").


%%--------------------------- Zookeeper Functions ---------------------
%% Return {ok, Reply}.

%% Reply = authed 
%% Returns {error, auth_in_progress}  if the authslot is already in use.
%% Returns {error, auth_failed} if server rejected auth
%% Returns {error, unknown, ErrorCodeBin} if something new happened
auth(ConnectionPId, Scheme, Id) ->
   ezk_connection:addauth(ConnectionPId, Scheme, Id).


%% Creates a new ZK_Node
%% Reply = Path where Path = String
create(ConnectionPId, Path, Data) ->
     ezk_connection:create(ConnectionPId, Path, Data).
n_create(ConnectionPId, Path, Data, Receiver, Tag) ->
     ezk_connection:n_create(ConnectionPId, Path, Data, Receiver, Tag).

%% Typ = e | s | es (stands for etheremal, sequenzed or both)
create(ConnectionPId, Path, Data, Typ) ->
    ezk_connection:create(ConnectionPId, Path, Data, Typ).
n_create(ConnectionPId, Path, Data, Typ, Receiver, Tag) ->
    ezk_connection:n_create(ConnectionPId, Path, Data, Typ, Receiver, Tag).


%% Acls = [Acl] where Acl = {Permissions, Scheme, Id} 
%% with Scheme and Id = String
%% and Permission = [Per] | String 
%% where Per = r | w | c | d | a
create(ConnectionPId, Path, Data, Typ, Acls)  ->
   ezk_connection:create(ConnectionPId, Path, Data, Typ, Acls).
n_create(ConnectionPId, Path, Data, Typ, Acls, Receiver, Tag)  ->
   ezk_connection:n_create(ConnectionPId, Path, Data, Typ, Acls, Receiver, Tag).

ensure_path(ConnectionPId, Path) ->
    ezk_connection:ensure_path(ConnectionPId, Path).

%% Deletes a ZK_Node
%% Only working if Node has no children.
%% Reply = Path where Path = String
delete(ConnectionPId, Path) ->
    ezk_connection:delete(ConnectionPId, Path).
n_delete(ConnectionPId, Path, Receiver, Tag) ->
    ezk_connection:n_delete(ConnectionPId, Path, Receiver, Tag).

%% Deletes a ZK_Node and all his childs.
%% Reply = Path where Path = String
delete_all(ConnectionPId, Path) ->
   ezk_connection:delete_all(ConnectionPId, Path).    

%% Looks if a Node exists
%% Reply = Parameters like in get (see one function below)
%% Can set a watch to the path
%% which is triggered 
%% a) when path is erased if path existed.
%% b) when path is created if path did not exist.
exists(ConnectionPId, Path) ->
    ezk_connection:exists(ConnectionPId, Path).
exists(ConnectionPId, Path, WatchOwner, WatchMessage) ->
    ezk_connection:exists(ConnectionPId, Path, WatchOwner, WatchMessage).

%% Reply = {Data, Parameters} where Data = The Data stored in the Node
%% and Parameters = {getdata, Czxid, Mzxid, Pzxid, Ctime, Mtime, Dataversion,
%%                   Datalength, Number_children, Cversion, Aclversion, Ephe_owner}
get(ConnectionPId, Path) ->
    ezk_connection:get(ConnectionPId, Path).
n_get(ConnectionPId, Path, Receiver, Tag) ->
    ezk_connection:n_get(ConnectionPId, Path, Receiver, Tag).
   
%% Like the one above but sets a datawatch to Path.
%% If watch is triggered a Message M is send to the PId WatchOwner
%% M = {WatchMessage, {Path, Type, SyncCon}}
%% with Type = child
get(ConnectionPId, Path, WatchOwner, WatchMessage) ->
    ezk_connection:get(ConnectionPId, Path, WatchOwner, WatchMessage).

%% Returns the actual Acls of a Node
%% Reply = {[ACL],Parameters} with ACl and Parameters like above
get_acl(ConnectionPId, Path) ->
    ezk_connection:get_acl(ConnectionPId, Path).
n_get_acl(ConnectionPId, Path, Receiver, Tag) ->
    ezk_connection:n_get_acl(ConnectionPId, Path, Receiver, Tag).

%% Sets new Data in a Node. Old ones are lost.
%% Reply = Parameters with Data like at get
set(ConnectionPId, Path, Data) ->
   ezk_connection:set(ConnectionPId, Path, Data).
n_set(ConnectionPId, Path, Data, Receiver, Tag) ->
   ezk_connection:n_set(ConnectionPId, Path, Data, Receiver, Tag).

%% Sets new Acls in a Node. Old ones are lost.
%% ACL like above.
%% Reply = Parameters with Data like at get
set_acl(ConnectionPId, Path, Acls) ->
    ezk_connection:set_acl(ConnectionPId, Path, Acls).
n_set_acl(ConnectionPId, Path, Acls, Receiver, Tag) ->
    ezk_connection:n_set_acl(ConnectionPId, Path, Acls, Receiver, Tag).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = [ChildName] where ChildName = <<"Name">>
ls(ConnectionPId, Path) ->
   ezk_connection:ls(ConnectionPId, Path).
n_ls(ConnectionPId, Path, Receiver, Tag) ->
   ezk_connection:n_ls(ConnectionPId, Path, Receiver, Tag).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls(ConnectionPId, Path, WatchOwner, WatchMessage) ->
    ezk_connection:ls(ConnectionPId, Path, WatchOwner, WatchMessage).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = {[ChildName],Parameters} with Parameters and ChildName like above.
ls2(ConnectionPId, Path) ->
   ezk_connection:ls2(ConnectionPId, Path).
n_ls2(ConnectionPId, Path, Receiver, Tag) ->
   ezk_connection:n_ls2(ConnectionPId, Path, Receiver, Tag).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls2(ConnectionPId, Path, WatchOwner, WatchMessage) ->
    ezk_connection:ls2(ConnectionPId, Path, WatchOwner, WatchMessage).

%% Returns the Actual Transaction Id of the Client.
%% Reply = Iteration = Int.
info_get_iterations(ConnectionPId) ->
    ezk_connection:info_get_iterations(ConnectionPId).
    
%% Starts a connection to a zookeeper Server
%% Returns {ok, PID} where Pid is the PId of the gen_server 
%% which manages the connection
start_connection() ->
    start_connection([]).
    
%% Starts a connection to a zookeeper Server
%% Returns {ok, PID} where Pid is the PId of the gen_server 
%% which manages the connection
start_connection(Servers) ->
    ezk_connection_manager:start_connection(Servers).
    
%% stops a connection. Returns ok.
end_connection(ConnectionPId, Reason) ->
    ezk_connection_manager:end_connection(ConnectionPId, Reason).

%% Adds new monitor PIds to bind to one connection. If one 
%% of the Monitors dies the connection is closed down.
add_monitors(ConnectionPId, Monitors) ->
    ezk_connection_manager:add_monitors(ConnectionPId, Monitors).

%% Provides a list of all actually active connections. 
%% Returns [Connection] where Connection = {PId, [MonitorPId]} 
get_connections() ->
    ezk_connection_manager:get_connections().

die(ConnectionPId) ->
    ezk:die(ConnectionPId, "No offence").

die(ConnectionPId, Reason) ->
    ezk_connection:die(ConnectionPId, Reason).
