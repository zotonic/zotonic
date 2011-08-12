%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% @doc Include a template, with possible caching
%%
%%      Example: include "some_file.tpl" and cache it for 3600 seconds
%%      {% include vary="something" maxage=3600 file="some_file.tpl" %}
%%
%%      Give a maxage of 0 for slam dunk protection but no caching.

%% Copyright 2009-2011 Marc Worrell
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

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, Vars, Context) ->
    File = proplists:get_value('$file', Params),
    AddC =  fun 
                ({Name,Value}, Vs) when Name =/= '$file' andalso Name =/= vary andalso Name =/= maxage ->
                    [{Name,Value}|Vs];
                (_, Vs) -> 
                    Vs
            end,
    Vars1 = lists:foldl(AddC, Vars, Params),

    Context1 = case proplists:get_value(sudo, Params) of
        true -> z_acl:sudo(Context);
        _ -> Context
    end,
    
    case proplists:get_value('$all', Params, false) of
        false ->
            {ok, z_template:render(File, Vars1, Context1)};
        true ->
            Templates = z_template:find_template(File, true, Context1),
            {ok, [ z_template:render(Tpl, Vars1, Context1) || Tpl <- Templates ]}
    end.
