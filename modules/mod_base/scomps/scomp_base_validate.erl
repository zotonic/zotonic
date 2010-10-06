%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% @doc Add a validation to an element

%% Copyright 2009-2010 Marc Worrell
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

-module(scomp_base_validate).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_scomp).
-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id       = proplists:get_value(id, Params, <<>>),
    TargetId = proplists:get_value(target,Params,Id),
    case proplists:get_all_values(on_form_invalid, Params) of
        [] ->
            {ok, z_render:validator(Id, TargetId, z_validation:rename_args(Params), Context)};
        OnInvalid ->
            ContextWire = lists:foldl(
                                fun (Act, Ctx) ->
                                    z_render:wire(Act, Ctx)
                                end,
                                z_context:new(Context),
                                lists:flatten(OnInvalid)),
            Script = iolist_to_binary(z_script:get_script(ContextWire)),
            {ok, z_script:add_script(["z_validation_on_invalid('",Id,"', function() {",Script,"});"], Context)}
   end.
