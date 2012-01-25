%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-16
%% @doc Replace the content of the target with the result of a render action.

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

-module(action_base_update).
-include("zotonic.hrl").
-export([
    render_action/4,
    render_update/5,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    render_update(update, TriggerId, TargetId, Args, Context).
    
render_update(Method, TriggerId, TargetId, Args, Context) ->
    Html = case proplists:get_value(template, Args) of
        undefined -> proplists:get_value(text, Args, "");
        Template -> #render{template=Template, vars=Args}
    end,
    case {TargetId, Html} of
        {undefined,_} -> render_inline(Method, TargetId, Html, Args, Context);
        {_,#render{}} -> render_postback(Method, TriggerId, TargetId, Html, Args, Context);
        {_,_} -> render_static(Method, TargetId, Html, Args, Context)
    end.

render_inline(Method, TargetId, Html, Args, Context) ->
    Sel = z_render:css_selector(TargetId, Args),
    case proplists:get_value(appear, Args) of
        true ->
            case Method of
                update        -> {[], z_render:appear_selector(Sel, Html, Context)};
		replace	      -> {[], z_render:appear_replace_selector(Sel, Html, Context)};
                insert_top    -> {[], z_render:appear_top_selector(Sel, Html, Context)};
                insert_bottom -> {[], z_render:appear_bottom_selector(Sel, Html, Context)};
		insert_before -> {[], z_render:appear_before_selector(Sel, Html, Context)};
		insert_after  -> {[], z_render:appear_after_selector(Sel, Html, Context)}
            end;
        _ -> 
            case Method of
                update        -> {[], z_render:update_selector(Sel, Html, Context)};
		replace       -> {[], z_render:replace_selector(Sel, Html, Context)};
                insert_top    -> {[], z_render:insert_top_selector(Sel, Html, Context)};
                insert_bottom -> {[], z_render:insert_bottom_selector(Sel, Html, Context)};
		insert_before -> {[], z_render:insert_before_selector(Sel, Html, Context)};
		insert_after  -> {[], z_render:insert_after_selector(Sel, Html, Context)}
            end
    end.

render_static(Method, TargetId, Html, Args, Context) ->
    Sel = z_render:css_selector(TargetId, Args),
    case proplists:get_value(appear, Args) of
        true -> 
            case Method of
                update        -> {z_render:appear_selector_js(Sel, Html), Context};
		replace       -> {z_render:appear_replace_selector_js(Sel, Html), Context};
                insert_top    -> {z_render:appear_top_selector_js(Sel, Html), Context};
                insert_bottom -> {z_render:appear_bottom_selector_js(Sel, Html), Context};
		insert_before -> {z_render:appear_before_selector_js(Sel, Html), Context};
		insert_after  -> {z_render:appear_after_selector_js(Sel, Html), Context}
            end;
        _ -> 
            case Method of 
                update        -> {z_render:update_selector_js(Sel, Html), Context};
		    replace   -> {z_render:replace_selector_js(Sel, Html), Context};
                insert_top    -> {z_render:insert_top_selector_js(Sel, Html), Context};
                insert_bottom -> {z_render:insert_bottom_selector_js(Sel, Html), Context};
		insert_before -> {z_render:insert_before_selector_js(Sel, Html), Context};
		insert_after  -> {z_render:insert_after_selector_js(Sel, Html), Context}
            end
    end.

render_postback(Method, TriggerId, TargetId, Html, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({render, Method, Html, Args}, undefined, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

event(#postback{message={render, Method, Html, Args}, target=TargetId}, Context) ->
    {[], Context1} = render_inline(Method, TargetId, Html, Args, Context),
    Context1.
