%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2015 Marc Worrell
%% @doc Perform some actions when an element comes into view.

%% Copyright 2011-2015 Marc Worrell
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

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, Vars, Context) ->
    case proplists:get_value(template, Params) of
        undefined ->
            Id = case proplists:get_value(id, Params) of
                     undefined -> binary_to_list(z_ids:identifier());
                     FixedId -> FixedId
                 end,
            Class  = proplists:get_value(class, Params, "z-lazy"),
            Image = proplists:get_value(image, Params, <<"/lib/images/spinner.gif">>),
            Params1 = [{id, Id}, {type, "visible"} | proplists:delete(id, Params)],
            Params2 = [ ensure_moreresults_visible(Param) || Param <- Params1 ],
            Html = [<<"<div id='">>, Id, <<"' class='">>, Class, <<"'><img src='">>, Image, <<"' alt='' /></div>">>],
            case scomp_base_wire:render(Params2, Vars, Context) of
                {ok, Result} -> {ok, [Html, render_state(Result)]};
                {error, _Reason} = Error -> Error
            end;
        _Template ->
            {TargetId, Html} = case proplists:get_value(target, Params) of
                undefined ->
                    Target = binary_to_list(z_ids:identifier()),
                    Div = [<<"<div id='">>, Target, <<"' class='z-lazy'><img src='/lib/images/spinner.gif' alt='' /></div>">>],
                    {Target, Div};
                Target ->
                    {Target, <<>>}
            end,
            Action = {update, [{target, TargetId}|Params]},
            case scomp_base_wire:render([{id, TargetId}, {type, "visible"}, {action, Action}], Vars, Context) of
                {ok, Result} -> {ok, [Html, render_state(Result)]};
                {error, _Reason} = Error -> Error
            end
    end.

render_state(#context{} = Context) ->
    z_context:get_render_state(Context);
render_state(MixedHtml) ->
    MixedHtml.

%% @doc Convenience function: the action "moreresults" is often used with the lazy scomp.
%%      A mistake that happens is to not add the 'visible' argument, which stops the
%%      moreresults from re-arming after the first time it is triggered.
ensure_moreresults_visible({action, {moreresults, _} = Action}) ->
    {action, ensure_moreresults_visible_1(Action)};
ensure_moreresults_visible({action, Actions}) when is_list(Actions) ->
    {action, [ ensure_moreresults_visible_1(Action) || Action <- Actions ]};
ensure_moreresults_visible(Arg) ->
    Arg.

ensure_moreresults_visible_1({moreresults, Args} = Action) ->
    case proplists:is_defined(visible, Args) of
        false -> {moreresults, [{visible,true}|Args]};
        true -> Action
    end;
ensure_moreresults_visible_1(Action) ->
    Action.
