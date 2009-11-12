%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Include a template, with possible caching
%%
%%      Example: include "some_file.tpl" and cache it for 3600 seconds
%%      {% @include depend="something" maxage=3600 file="some_file.tpl" %}
%%
%%      Give a maxage of 0 for slam dunk protection but no caching.

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

-module(scomp_base_include).
-behaviour(gen_scomp).

-export([init/1, varies/2, terminate/2, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(Params, _Context) -> 
    MaxAge = proplists:get_value(maxage, Params),
    case z_convert:to_integer(MaxAge) of
        undefined -> 
            undefined; 
        Max ->
            Vary    = proplists:get_all_values(vary, Params),
            Params1 = proplists:delete(maxage, Params),
            Params2 = proplists:delete(vary, Params1),
            {Params2, Max, Vary}
    end.

terminate(_State, _Context) -> ok.

render(Params, Vars, Context, _State) ->
    File = proplists:get_value(file, Params),
    AddC =  fun 
                ({Name,Value}, Vs) when Name =/= file andalso Name =/= vary andalso Name =/= maxage ->
                    [{Name,Value}|Vs];
                (_, Vs) -> 
                    Vs
            end,
    Vars1 = lists:foldl(AddC, Vars, Params),
    {ok, z_template:render(File, Vars1, Context)}.

