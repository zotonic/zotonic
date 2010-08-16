%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-06
%% @doc Insert the result of a render action at the top of an HTML element.

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

-module(action_base_insert_top).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Html = case proplists:get_value(template, Args) of
        undefined -> proplists:get_value(text, Args, "");
        Template -> #render{template=Template, vars=Args}
    end,
    case proplists:get_value(appear, Args) of
        true -> {[], z_render:appear_top_selector(z_render:css_selector(TargetId, Args), Html, Context)};
        _ -> {[], z_render:insert_top_selector(z_render:css_selector(TargetId, Args), Html, Context)}
    end.
