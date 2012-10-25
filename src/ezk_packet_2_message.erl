%% -------------------------------------------------------------------
%%
%% ezk_packet_2_message: A module that contains functions to convert
%%                       incoming binary messages into Erlangdata.
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
-module(ezk_packet_2_message).
-export([get_message_typ/1, replymessage_2_reply/3, get_watch_data/1]).
-include_lib("../include/ezk.hrl").

-record(getdata, {czxid,
		  mzxid,
		  pzxid,
		  ctime,
		  mtime,
		  dataversion,
		  datalength,
		  number_children,
		  cversion,
		  aclversion,
		  ephe_owner}).
		  

%% First stage of Message Passing.
%% The first part of the Message determines the type (heartbeat, watchevent, reply) and
%% the first part of the Header (which is necessary to find the right entry in 
%% open_requests) if it is a reply. 
%% Returns {heartbeat, HeartbeatBin} | {watchevent, Payload} 
%%       | {normal, MessageId, Zxid, Payload}
get_message_typ(Data) ->
    case Data  of
%%% Heartbeat
        <<255,255,255,254, Heartbeat/binary>> ->
            {heartbeat, Heartbeat};
%%% Watchevents
	<<255,255,255,255, 255,255,255,255, 255,255,255,255 , 0,0,0,0, Payload/binary>> ->
	    ?LOG(3, "packet_2_message: A Watchevent arrived"),
	    {watchevent, Payload};
	<<255, 255, 255, 252, 0:64, Payload/binary>> ->
	    {authreply, Payload};
%%% Normal Replys
        <<MessId:32, Zxid:64, Payload/binary>> ->
	    ?LOG(3, "packet_2_message: A normal Message arrived"),
            {normal, MessId, Zxid, Payload}
    end.

%% A message typed as watchevent is processed
%% returns {child, Path, SyncConnected} | {data, Path, SyncConnected}
get_watch_data(Binary) ->
     <<TypInt:32, SyncConnected:32, PackedPath/binary>> = Binary,
     {Path, _Nothing} = unpack(PackedPath),
     case TypInt of
	 1 -> 
	     Typ = node_created;
	 2 -> 
	     Typ = node_deleted;
	 3 -> 
	     Typ = data_changed;
	 4 -> 
	     Typ = child_changed
     end,   
     {Typ, binary_to_list(Path), SyncConnected}.

%% Gets a replybinary from the server and returns it as a parsed Erlang tupel.
%% First step is to filter if there was an error and pass it on to the server if there is.
%% If not the interpret_reply_data function is used to interpret the Payload. 
replymessage_2_reply(CommId, Path, PayloadWithErrorCode) ->
    ?LOG(1,"packet_2_message: Trying to Interpret payload: ~w", [PayloadWithErrorCode]),
    case PayloadWithErrorCode of
	<<0,0,0,0,Payload/binary>> -> 
	    ?LOG(1
		 ,"packet_2_message: Interpreting the payload ~w with commid ~w and Path ~w"
		 ,[Payload, CommId, Path]),
            Replydata = interpret_reply_data(CommId, Path, Payload),
	    Reply = {ok, Replydata},
	    ?LOG(1, "The Reply is ~w",[Reply]);
	<<255,255,255,142,_Payload/binary>> ->
	    Reply = {error, inval_acl};
	<<255,255,255,146,_Payload/binary>> ->
	    Reply = {error, dir_exists};
	<<255,255,255,154,_Payload/binary>> ->
	    Reply = {error, no_rights};   
	<<255,255,255,155,_Payload/binary>> ->
	    Reply = {error, no_dir};   
	<<255,255,255,248,_Payload/binary>> ->
	    Reply = {error, childs_or_forbidden};   
	Cody -> 
	    Reply = {unknown, Cody}
    end,      
    Reply.

%% There is a pattern matching on the command id and depending on the command id
%% the Reply is interpreted. 
%%% create --> Reply = The new Path
interpret_reply_data(1, _Path, Reply) ->
    <<LengthOfData:32, Data/binary>> = Reply,
    {ReplyPath, _Left} = split_binary(Data, LengthOfData),
    binary_to_list(ReplyPath); 
%%% delete --> Reply = Nothing --> use the Path
interpret_reply_data(2, Path, _Reply) ->
    Path;
%%% exists
interpret_reply_data(3, _Path, Reply) ->
    getbinary_2_list(Reply);
%%% get --> Reply = The data stored in the node and then all the nodes  parameters
interpret_reply_data(4, _Path, Reply) -> 
    ?LOG(3,"P2M: Got a get reply"),
    <<LengthOfData:32, Data/binary>> = Reply,
    ?LOG(3,"P2M: Length of data is ~w",[LengthOfData]),
    {ReplyData, Left} = split_binary(Data, LengthOfData),
    ?LOG(3,"P2M: The Parameterdata is ~w",[Left]),    
    ?LOG(3,"P2M: Data is ~w",[ReplyData]),    
    Parameter = getbinary_2_list(Left),
    {ReplyData, Parameter};
%%% set --> Reply = the nodes parameters
interpret_reply_data(5, _Path, Reply) -> 
    getbinary_2_list(Reply);	       

%%% get_acl --> A list of the Acls and the nodes parameters
interpret_reply_data(6, _Path, Reply) ->
    ?LOG(3,"P2M: Got a get acl reply"),
    <<NumberOfAcls:32, Data/binary>> = Reply,
    ?LOG(3,"P2M: There are ~w acls",[NumberOfAcls]),
    {Acls, Data2} = get_n_acls(NumberOfAcls, [],  Data),
    ?LOG(3,"P2M: Acls got parsed: ~w", [Acls]),
    Parameter = getbinary_2_list(Data2),
    ?LOG(3,"P2M: Data got also parsed."),
    {Acls, Parameter};
%%% set_acl --> Reply = the nodes parameters
interpret_reply_data(7, _Path, Reply) ->
    getbinary_2_list(Reply);
%%% ls --> Reply = a list of all children of the node.
interpret_reply_data(8, _Path, Reply) ->
    ?LOG(4,"packet_2_message: Interpreting a ls"),
    <<NumberOfAnswers:32, Data/binary>> = Reply,
    ?LOG(4,"packet_2_message: Number of Children: ~w",[NumberOfAnswers]),
    ?LOG(4,"packet_2_message: The Binary is: ~w",[Data]),
    {List, _Left} =  get_n_paths(NumberOfAnswers, Data),
    ?LOG(4,"packet_2_message: Paths extracted."),
    ?LOG(4,"packet_2_message: Paths are: ~w",[List]),    
    lists:map(fun(A) -> list_to_binary(A) end, List);
%%% ls2 --> Reply = a list of the nodes children and the nodes parameters
interpret_reply_data(12, _Path, Reply) ->
    {<<NumberOfAnswers:32>>, Data} = split_binary(Reply, 4),
    {Children, Left} =  get_n_paths(NumberOfAnswers, Data),
    Parameter = getbinary_2_list(Left),
    [{children, Children}|Parameter].

%%----------------------------------------------------------------
%% Little Helpers (internally neede functions)
%%----------------------------------------------------------------

%% unpacks N paths from a Binary.
%% Returns {ListOfPaths , Leftover}
get_n_paths(0, Binary) ->    
    {[],Binary};
get_n_paths(N, Binary) ->
    {ThisPathBin, ToProcessBin} = unpack(Binary),
    {RekResult, Left2} = get_n_paths(N-1, ToProcessBin),
    {[binary_to_list(ThisPathBin) | RekResult ], Left2}.

%% interprets the parameters of a node and returns a List of them.
getbinary_2_list(Binary) ->
    ?LOG(3,"p2m: Trying to match Parameterdata"),
    <<Czxid:64,                           Mzxid:64,
      Ctime:64,                           Mtime:64,
      DaVer:32,          CVer:32,         AclVer:32,    EpheOwner:64,  
                         DaLe:32,         NumChi:32,    Pzxid:64>> = Binary,
    ?LOG(3,"p2m: Matching Parameterdata Successfull"),
    #getdata{czxid          = Czxid,   mzxid     = Mzxid,
	     ctime          = Ctime,   mtime     = Mtime,
	     dataversion    = DaVer,   datalength= DaLe,
	     number_children= NumChi,  pzxid     = Pzxid,
	     cversion       = CVer,    aclversion= AclVer,
	     ephe_owner     = EpheOwner}.

%% uses the first 4 Byte of a binary to determine the lengths of the data and then 
%% returns a pair {Data, Leftover}
unpack(Binary) ->
    <<Length:32, Load/binary>> = Binary,
    split_binary(Load, Length).

%% Looks for the first N Acls in a Binary.
%% Returns {ListOfAclTripels, Leftover}
get_n_acls(0, Acls, Binary) ->
    ?LOG(3,"P2M: Last Acl parsed."),
    {Acls, Binary};
get_n_acls(N, Acls,  Binary) ->
    ?LOG(3,"P2M: Parse next acl from this: ~w.",[Binary]),
    <<0:27, A:1, D:1, C:1, W:1, R:1, Left/binary>>  = Binary,
    {Scheme, Left2}         = unpack(Left),
    ?LOG(3,"P2M: Scheme is: ~w.",[Scheme]),
    {Id,     NowLeft}       = unpack(Left2),
    ?LOG(3,"P2M: Id is: ~w.",[Id]),
    ?LOG(3,"P2M: The Permissiontupel is: ~w.",[{R,W,C,D,A}]),
    Permi = get_perm_from_tupel({R,W,C,D,A}),
    NewAcls = [{Permi, Scheme, Id} | Acls ],    
    get_n_acls(N-1, NewAcls, NowLeft).

%% Interprets the Permissions of an Acl.
get_perm_from_tupel({1,W,C,D,A}) ->
    [r | get_perm_from_tupel({0,W,C,D,A})];
get_perm_from_tupel({0,1,C,D,A}) ->
    [w | get_perm_from_tupel({0,0,C,D,A})];
get_perm_from_tupel({0,0,1,D,A}) ->
    [c | get_perm_from_tupel({0,0,0,D,A})];
get_perm_from_tupel({0,0,0,1,A}) ->
    [d | get_perm_from_tupel({0,0,0,0,A})];
get_perm_from_tupel({0,0,0,0,1}) ->
    [a | get_perm_from_tupel({0,0,0,0,0})];
get_perm_from_tupel({0,0,0,0,0}) ->
    [].
