%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Perform some actions when an element comes into view.

%% Copyright 2011 Marc Worrell
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

-module(scomp_base_lazy).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, Vars, Context) ->
    Id = case proplists:get_value(id, Params) of
             undefined -> z_ids:identifier();
             FixedId -> FixedId
         end,
    Class  = proplists:get_value(class, Params, "z-lazy"),
    Image = proplists:get_value(image, Params, <<"/lib/images/spinner.gif">>),
    Params1 = [{id, Id}, {type, "visible"} | proplists:delete(id, Params)],
    Html = [<<"<div id='">>, Id, <<"' class='">>, Class, <<"'><img src='">>, Image, <<"' alt='' /></div>">>],
    case scomp_base_wire:render(Params1, Vars, Context) of
        {ok, Result} -> {ok, [Html, z_context:prune_for_template(Result)]};
        {error, _Reason} = Error -> Error
    end.
