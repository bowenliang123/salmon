
%% -------------------------------------------------------------------
%%
%% ezk_sup: The supervisor. Starts the connection and restarts it if it's broken.
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

-module(ezk_sup).

-behaviour(supervisor).
-include_lib("../include/ezk.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    ?LOG(1,"args: ~w",[Args]),
    ?LOG(1,"Supervisor: making Childspec."),
%%% The only child is the Connection.
    Connection = ?CHILD(ezk_connection_manager, worker, [Args]),
    ChildSpec = [Connection],
    ?LOG(1,"Supervisor: done Childspec: ~w.", [ChildSpec]),
    {ok, { {one_for_one, 500, 10}, ChildSpec} }.

