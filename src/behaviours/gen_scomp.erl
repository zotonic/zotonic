%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Copyright 2009 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Scomp behaviour definition.  A scomp is a screen component that can optionally be cached.
%%      
%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Vars, Context, State) -> {ok, NewContext} | {ok, io_list()} | {error, Error}
%%      terminate(State, Context) -> ok
%%      
%%      	State = term()
%%      	Params = proplist()
%%      	Context = context()
%%      
%%      varies(Params, Context) -> {EssentialParams, MaxAge, Varies} | undefined
%%      
%%      	Params = proplist()
%%          EssentialParams = proplist()
%%      	MaxAge = integer()
%%          Varies = TermList  (used as dependencies for the depcache)


-module(gen_scomp).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {init,1},
        {render, 4},
        {terminate, 2},
        {varies, 2}
     ];
behaviour_info(_Other) ->
    undefined.
