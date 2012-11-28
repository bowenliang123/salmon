%% @author simonlbw
%% @doc @todo Add description to sm_basic_bolt.

-module(sm_basic_bolt).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([behaviour_info/1]).  
-export([init/1, nextTuple/3]).

behaviour_info(callbacks) ->  
    [{init,0},
	 {nextTuple,1}];  
behaviour_info(_Other) ->  
    undefined.  


init(Mod) ->
	State = Mod:init().

nextTuple(Mod, Tuple, State)->
	{ok,Tuple1,State1} = Mod:nextTuple(Tuple, State).
	
	
%% ====================================================================
%% Internal functions
%% ====================================================================


  


  
  
