%% @copyright 2014 Maas-Maarten Zeeman
%% @doc Create modular, dynamic user interfaces with components and rich interactions.

%% Copyright 2014 Maas-Maarten Zeeman
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

-module(mod_component).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Component Framework").
-mod_description("Create modular, dynamic user interfaces with components and rich interactions.").
-mod_prio(9000).
-mod_depends([mod_mqtt]).
-mod_provides([component]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("include/mod_component.hrl").

-export([event/2]).

%%
%% Incoming request to dynamically load a component on a page.
%%
%% We send a inject_component notification.
%%
%% When we get an answer back we figure out what js/css or possibly other componets
%% need to be loaded, and inject a script on the page which downloades the resoures.
%%
%% When everything is loaded, the init function is called.
%%
event(_, Context) ->
    Context.
% event(#z_msg_v1{data=#load_component{name=Name, loaded=LoadedResources}}, Context) ->
%     case z_notifier:first(#inject_component{name=Name}, Context) of
%         undefined ->
%             %% TODO send a nack to the page instead.
%             {Script, Ctx1} = z_script:split(z_render:growl("Component not found", Context)),
%             z_transport:page(javascript, Script, [{qos, 1}], Context),
%             Ctx1;
%         {ok, InitScript, Resources} ->
%             Required = unify(collect(Resources)),
%             Loaded = unify(uncollapse(collect(LoadedResources))),

%             % Calculate which resources must be loaded.
%             Needed = subtract(Required, Loaded),

%             %% Wire an inject script.
%             Ctx1 = z_render:wire({inject, [{name, Name},
%                         {init_script, InitScript},
%                         {files, Needed}]}, Context),
%             {Script, CleanContext} = z_script:split(Ctx1),
%             z_transport:page(javascript, Script, [{qos, 1}], CleanContext),
%             CleanContext
%     end.

%%
%% Helpers
%%

collect(List) ->
    collect(List, []).

collect([], Acc) ->
    lists:reverse(lists:flatten(Acc));
collect([{Type, Resources}|Rest], Acc)
        when Type =:= js orelse Type =:= css
        orelse Type =:= <<"js">> orelse Type =:= <<"css">> ->
    collect(Rest, [Resources | Acc]);
collect([Resource|Rest], Acc) ->
    collect(Rest, [Resource | Acc]).

uncollapse(L) ->
    uncollapse(L, []).

uncollapse([], Acc) ->
    lists:flatten(Acc);
uncollapse([H|T], Acc) ->
    U = z_lib_include:uncollapse(H),
    uncollapse(T, [U|Acc]).

% @doc Return a list with resources which must be loaded.
subtract(R, L) ->
    Loaded = sets:from_list(L),
    Required = sets:from_list(R),
    Needed = sets:subtract(Required, Loaded),
    sets:to_list(Needed).

%% Makes strips a leading lib from the path
unify(List) ->
    unify(List, []).

unify([], Acc) ->
    lists:reverse(Acc);
unify([<<"/lib", Rest/binary>>|T], Acc) ->
    unify(T, [Rest|Acc]);
unify([H|T], Acc) ->
    unify(T, [H|Acc]).

