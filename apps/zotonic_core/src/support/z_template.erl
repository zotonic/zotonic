%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Template handling, compiles and renders django compatible templates using the
%% template_compiler.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

    is_template_module/1,
    template_module/3,
    blocks/3,
    includes/3,
    extends/3
]).

-include_lib("template_compiler/include/template_compiler.hrl").
-include("../../include/zotonic.hrl").


start_link(Site) ->
    Context = z_context:new(Site),
    z_context:logger_md(Context),
    z_notifier:observe(module_reindexed, {?MODULE, module_reindexed}, Context),
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
            <<"z_language">>,
            <<"extra_args">>
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
        {error, Reason} when is_list(Reason); is_binary(Reason) ->
            try
                Reason1 = iolist_to_binary(Reason),
                ?LOG_ERROR(#{
                    text => <<"Error rendering template">>,
                    in => zotonic_core,
                    template => Template,
                    result => error,
                    reason => Reason1
                })
            catch
                _:_ ->
                    ?LOG_ERROR(#{
                        text => <<"Error rendering template">>,
                        in => zotonic_core,
                        template => Template,
                        result => error,
                        reason => Reason
                    })
            end,
            <<>>;
        {error, Reason} when is_map(Reason) ->
            ?LOG_ERROR(Reason),
            <<>>;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error rendering template">>,
                in => zotonic_core,
                template => Template,
                result => error,
                reason => Reason
            }),
            <<>>
    end.

props_to_map(L, Map) ->
    lists:foldr(
        fun
            ({K, V}, Acc) ->
                Acc#{ K => V };
            (K, Acc) ->
                Acc#{ K => true }
        end,
        Map,
        L).

%% @todo Remove these functions, templates should not have any javascript etc. (call z_render for old style templates)
%% @doc Render a template to an iolist().  This removes all scomp state etc from the rendered html and appends the
%% information in the scomp states to the context for later rendering.
-spec render_to_iolist(template_compiler:template() | #module_index{},
    list() | map(), z:context()) -> {iolist(), z:context()}.
render_to_iolist(File, Vars, Context) ->
    Html = render(File, Vars, Context),
    z_render:render_to_iolist(Html, Context).

%% @doc Render a block template to an iolist().
-spec render_block_to_iolist(atom(), template_compiler:template(), list()|map(), z:context()) ->
    {iolist(), z:context()}.
render_block_to_iolist(Block, File, Vars, Context) ->
    Html = render_block(Block, File, Vars, Context),
    z_render:render_to_iolist(Html, Context).

%% @doc Check if the modulename looks like a module generated by the template compiler.
-spec is_template_module(binary()|string()|atom()) -> boolean().
is_template_module(Module) ->
    template_compiler:is_template_module(Module).


%% @doc Return the module of a compiled template. This will compile the template if it was not
%% yet compiled. Vars is needed for catinclude expands.
-spec template_module(Template, Vars, Context) -> {ok, Module} | {error, term()} when
    Template :: template_compiler:template() | #module_index{},
    Vars :: list() | map(),
    Context :: z:context(),
    Module :: module().
template_module(#module_index{filepath=Filename, key=Key}, Vars, Context) ->
    Template = #template_file{
        filename=Filename,
        template=Key#module_index_key.name
    },
    template_module(Template, Vars, Context);
template_module(#template_file{ filename = Filename }, Vars, Context) when is_map(Vars) ->
    Opts =  [
        {runtime, z_template_compiler_runtime},
        {context_name, z_context:site(Context)},
        {context_vars, [
            <<"sudo">>,
            <<"anondo">>,
            <<"z_language">>,
            <<"extra_args">>
        ]}
    ],
    template_compiler:lookup(Filename, Opts, Context);
template_module(Template, Vars, Context) ->
    case z_template_compiler_runtime:map_template(Template, Vars, Context) of
        {ok, MappedTemplate} ->
            template_module(MappedTemplate, Vars, Context);
        {error, _} = Error ->
            Error
    end.


%% @doc Return the list of all block names in a template. This only returns the list
%% in the current template and not in the extended or overruled templates.  Vars is
%% needed for catinclude expands.
-spec blocks(Template, Vars, Context) -> {ok, Blocks} | {error, term()} when
    Template :: template_compiler:template() | #module_index{},
    Vars :: list() | map(),
    Context :: z:context(),
    Blocks :: list( atom() ).
blocks(Template, Vars, Context) ->
    case template_module(Template, Vars, Context) of
        {ok, Module} ->
            {ok, Module:blocks()};
        {error, _} = Error ->
            Error
    end.

%% @doc Return the list of all includes with a fixed template string in a template.
%% Vars is needed for catinclude expands.
-spec includes(Template, Vars, Context) -> {ok, Includes} | {error, term()} when
    Template :: template_compiler:template() | #module_index{},
    Vars :: list() | map(),
    Context :: z:context(),
    Includes :: list( map() ).
includes(Template, Vars, Context) ->
    case template_module(Template, Vars, Context) of
        {ok, Module} ->
            {ok, Module:includes()};
        {error, _} = Error ->
            Error
    end.

%% @doc Return the template that the given template extends or overrules.
-spec extends(Template, Vars, Context) -> {ok, Extends} | {error, term()} when
    Template :: template_compiler:template() | #module_index{},
    Vars :: list() | map(),
    Context :: z:context(),
    Extends :: undefined | binary() | overrules.
extends(Template, Vars, Context) ->
    case template_module(Template, Vars, Context) of
        {ok, Module} ->
            {ok, Module:extends()};
        {error, _} = Error ->
            Error
    end.

