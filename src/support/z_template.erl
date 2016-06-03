%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell
%% @doc Template handling, compiles and renders django compatible templates using the template_compiler

%% Copyright 2009-2016 Marc Worrell
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

-module(z_template).
-author("Marc Worrell <marc@worrell.nl>").

-compile([{parse_transform, lager_transform}]).

-export([start_link/1]).

-include_lib("template_compiler/include/template_compiler.hrl").
-include_lib("zotonic.hrl").

%% External exports
-export([
    reset/1,
    module_reindexed/2,
    render/2,
    render/3,
    render_to_iolist/3,
    is_template_module/1
]).


start_link(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    z_notifier:observe(module_reindexed, {?MODULE, module_reindexed}, z_context:new(Host)),
    ignore.


%% @doc Force a reset of all templates, used after a module has been activated or deactivated.
-spec reset(atom()|#context{}) -> ok.
reset(Host) when is_atom(Host) ->
    z_filewatcher_mtime:flush_site(Host),
    lists:foreach(
            fun(UA) ->
                template_compiler:flush_context_name({Host, UA})
            end,
            z_user_agent:classes());
reset(Context) ->
    reset(z_context:site(Context)).

%% @doc Observer, triggered when there are new module files indexed
-spec module_reindexed(module_reindexed, #context{}) -> ok.
module_reindexed(module_reindexed, Context) ->
    reset(z_context:site(Context)).

-spec render(#render{}, #context{}) -> template_compiler:render_result().
render(#render{template=Template, vars=Vars}, Context) ->
    render(Template, Vars, Context).

-spec render(template_compiler:template()|#module_index{}, list()|#{}, #context{}) -> template_compiler:render_result().
render(Template, Vars, Context) when is_list(Vars) ->
    render(Template, props_to_map(Vars, #{}), Context);
render(#module_index{filepath=Filename, key=Key}, Vars, Context) ->
    Template = #template_file{
        filename=Filename, 
        template=Key#module_index_key.name
    },
    render(Template, Vars, Context);
render(Template, Vars, Context) when is_map(Vars) ->
    OldCaching = z_depcache:in_process(true),
    Vars1 = ensure_zotonic_vars(Vars, Context),
    Result = template_compiler:render(
                    Template,
                    Vars1,
                    [
                        {runtime, z_template_compiler_runtime},
                        {context_vars, [
                            <<"sudo">>,
                            <<"anondo">>,
                            <<"z_language">>
                        ]}
                    ],
                    Context),
    z_depcache:in_process(OldCaching),
    case Result of
        {ok, Output} ->
            Output;
        {error, {{ErrFile,Line,Col}, _YeccModule, Error}} ->
            try 
                Error1 = iolist_to_binary(Error),
                lager:error("[~p] Error rendering template: ~s:~p (~s)~n",
                           [z_context:site(Context), ErrFile, Line, Col, Error1])
            catch 
                _:_ ->
                    lager:error("[~p] Error rendering template: ~s:~p (~p)~n",
                               [z_context:site(Context), ErrFile, Line, Col, Error])
            end,
            <<>>;
        {error, Reason} when is_list(Reason); is_binary(Reason) ->
            try 
                Reason1 = iolist_to_binary(Reason),
                lager:error("[~p] Error rendering template: ~s (~s)~n",
                           [z_context:site(Context), Template, Reason1])
            catch 
                _:_ ->
                    lager:error("[~p] Error rendering template: ~s (~p)~n",
                               [z_context:site(Context), Template, Reason])
            end,
            <<>>;
        {error, _} = Error ->
            lager:info("[~p] template render of ~p returns ~p",
                       [z_context:site(Context), Template, Error]),
            <<>>
    end.

ensure_zotonic_vars(Vars, Context) ->
    case maps:get(z_language, Vars, undefined) of
        undefined -> Vars#{z_language => z_context:language(Context)};
        _ -> Vars
    end.

props_to_map([], Map) -> 
    Map;
props_to_map([{K,V}|Rest], Map) ->
    props_to_map(Rest, Map#{K => V});
props_to_map([K|Rest], Map) ->
    props_to_map(Rest, Map#{K => true}).


%% @doc Render a template to an iolist().  This removes all scomp state etc from the rendered html and appends the
%% information in the scomp states to the context for later rendering.
-spec render_to_iolist(template_compiler:template(), list()|#{}, #context{}) -> {iolist(), #context{}}.
render_to_iolist(File, Vars, Context) ->
    Html = render(File, Vars, Context),
    z_render:render_to_iolist(Html, Context).

%% @doc Check if the modulename looks like a module generated by the template compiler.
-spec is_template_module(binary()|string()|atom()) -> boolean().
is_template_module(Module) ->
    template_compiler:is_template_module(Module).

