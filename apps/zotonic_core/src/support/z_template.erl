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

%% External exports
-export([
    reset/1,
    module_reindexed/2,
    render/2,
    render/3,
    render_to_iolist/3,

    render_block/3,
    render_block/4,
    render_block_to_iolist/4,

    is_template_module/1
]).

-include_lib("template_compiler/include/template_compiler.hrl").
-include("../../include/zotonic.hrl").



start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    z_notifier:observe(module_reindexed, {?MODULE, module_reindexed}, z_context:new(Site)),
    ignore.


%% @doc Force a reset of all templates, used after a module has been activated or deactivated.
-spec reset(atom()|#context{}) -> ok.
reset(Site) when is_atom(Site) ->
    z_file_mtime:flush_site(Site),
    template_compiler:flush_context_name(Site);
reset(Context) ->
    reset(z_context:site(Context)).

%% @doc Observer, triggered when there are new module files indexed
-spec module_reindexed(module_reindexed, #context{}) -> ok.
module_reindexed(module_reindexed, Context) ->
    reset(z_context:site(Context)).

-spec render(#render{}, #context{}) -> template_compiler:render_result().
render(#render{template=Template, vars=Vars}, Context) ->
    render_block(undefined, Template, Vars, Context).

-spec render(template_compiler:template()|#module_index{}, list()|map(), #context{}) -> template_compiler:render_result().
render(Template, Vars, Context) ->
    render_block(undefined, Template, Vars, Context).

-spec render_block(atom(), #render{}, #context{}) -> template_compiler:render_result().
render_block(Block, #render{template=Template, vars=Vars}, Context) ->
    render_block(Block, Template, Vars, Context).

-spec render_block(atom(), template_compiler:template()|#module_index{}, list()|map(), #context{}) -> template_compiler:render_result().
render_block(OptBlock, Template, Vars, Context) when is_list(Vars) ->
    render_block(OptBlock, Template, props_to_map(Vars, #{}), Context);
render_block(OptBlock, #module_index{filepath=Filename, key=Key}, Vars, Context) ->
    Template = #template_file{
        filename=Filename,
        template=Key#module_index_key.name
    },
    render_block(OptBlock, Template, Vars, Context);
render_block(OptBlock, Template, Vars, Context) when is_map(Vars) ->
    OldCaching = z_depcache:in_process(true),
    Opts =  [
        {runtime, z_template_compiler_runtime},
        {context_name, z_context:site(Context)},
        {context_vars, [
            <<"sudo">>,
            <<"anondo">>,
            <<"z_language">>
        ]}
    ],
    Result = case OptBlock of
                undefined ->
                    template_compiler:render(Template, Vars, Opts, Context);
                Block when is_atom(Block) ->
                    template_compiler:render_block(Block, Template, Vars, Opts, Context)
             end,
    z_depcache:in_process(OldCaching),
    case Result of
        {ok, Output} ->
            Output;
        {error, {{ErrFile, Line, Col}, _YeccModule, Error}} ->
            try
                Error1 = iolist_to_binary(Error),
                lager:error(
                    "Error rendering template ~s:~p:~p due to ~s~n",
                    [ErrFile, Line, Col, Error1]
                )
            catch
                _:_ ->
                    lager:error(
                        "Error rendering template ~s:~p:~p due to ~p~n",
                        [ErrFile, Line, Col, Error]
                    )
            end,
            <<>>;
        {error, Reason} when is_list(Reason); is_binary(Reason) ->
            try
                Reason1 = iolist_to_binary(Reason),
                lager:error(
                    "Error rendering template ~s due to ~s~n",
                    [Template, Reason1]
                )
            catch
                _:_ ->
                    lager:error(
                        "Error rendering template ~s due to ~p~n",
                        [Template, Reason]
                    )
            end,
            <<>>;
        {error, _} = Error ->
            lager:info("template render of ~p returns ~p", [Template, Error]),
            <<>>
    end.

props_to_map([], Map) ->
    Map;
props_to_map([{K,V}|Rest], Map) ->
    props_to_map(Rest, Map#{K => V});
props_to_map([K|Rest], Map) ->
    props_to_map(Rest, Map#{K => true}).


%% @todo Remove these functions, templates should not have any javascript etc. (call z_render for old style templates)
%% @doc Render a template to an iolist().  This removes all scomp state etc from the rendered html and appends the
%% information in the scomp states to the context for later rendering.
-spec render_to_iolist(template_compiler:template() | #module_index{},
    list() | map(), #context{}) -> {iolist(), #render_state{}}.
render_to_iolist(File, Vars, Context) ->
    Html = render(File, Vars, Context),
    z_render:render_to_iolist(Html, Context).

%% @doc Render a block template to an iolist().
-spec render_block_to_iolist(atom(), template_compiler:template(), list()|map(), #context{}) -> {iolist(), #render_state{}}.
render_block_to_iolist(Block, File, Vars, Context) ->
    Html = render_block(Block, File, Vars, Context),
    z_render:render_to_iolist(Html, Context).

%% @doc Check if the modulename looks like a module generated by the template compiler.
-spec is_template_module(binary()|string()|atom()) -> boolean().
is_template_module(Module) ->
    template_compiler:is_template_module(Module).

