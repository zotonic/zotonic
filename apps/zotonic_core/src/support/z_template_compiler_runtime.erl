%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2024 Marc Worrell
%% @doc Runtime for the compiled templates with Zotonic specific interfaces.
%% @end

%% Copyright 2016-2024 Marc Worrell
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

-module(z_template_compiler_runtime).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(template_compiler_runtime).

-export([
    map_template/3,
    map_template_all/3,
    is_modified/3,
    compile_map_nested_value/3,
    find_nested_value/3,
    find_nested_value/4,
    find_value/4,
    get_context_name/1,
    set_context_vars/2,
    get_translations/2,
    lookup_translation/3,
    model_call/4,
    custom_tag/4,
    builtin_tag/5,
    cache_tag/6,
    javascript_tag/3,
    spaceless_tag/3,
    to_bool/2,
    to_list/2,
    to_simple_values/2,
    to_simple_value/2,
    to_render_result/3,
    escape/2,
    trace_compile/4,
    trace_render/3,
    trace_block/4
    ]).

-include_lib("../../include/zotonic.hrl").
-include_lib("../../include/zotonic_release.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").


%% @doc Dynamic mapping of a template to a template name, context sensitive on the template vars.
-spec map_template(template_compiler:template(), map(), term()) ->
            {ok, template_compiler:template_file()} | {error, enoent|term()}.
map_template(#template_file{} = Tpl, _Vars, _Context) ->
    {ok, Tpl};
map_template({cat, Template}, #{ '$cat' := Cats }, Context) when is_list(Cats) ->
    map_template_cat(Template, Cats, Context);
map_template({cat, Template}, #{ 'id' := Id } = Vars, Context) ->
    map_template({cat, Template, Id}, Vars, Context);
map_template({cat, Template}, #{ <<"id">> := Id } = Vars, Context) ->
    map_template({cat, Template, Id}, Vars, Context);
map_template({cat, Template}, _Vars, Context) ->
    map_template_1(Template, Context);
map_template({cat, Template, [Cat|_] = IsA}, _Vars, Context) when is_atom(Cat); is_binary(Cat); is_list(Cat) ->
    map_template_cat(Template, IsA, Context);
map_template({cat, Template, Id}, _Vars, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> map_template_1(Template, Context);
        RscId -> map_template_cat(Template, RscId, Context)
    end;
map_template({overrules, Template, Filename}, _Vars, Context) ->
    Templates = z_module_indexer:find_all(template, Template, Context),
    case find_next_template(Filename, Templates) of
        {ok, _} = Ok ->
            Ok;
        {error, enoent} ->
            ?LOG_WARNING(#{
                text => <<"No template for overrules">>,
                in => zotonic_core,
                template => Template,
                filename => Filename
            }),
            {error, enoent}
    end;
map_template(Template, _Vars, Context) ->
    map_template_1(Template, Context).


find_next_template(_Filename, []) ->
    {error, enoent};
find_next_template(Filename, [#module_index{filepath=Filename},Next|_]) ->
    Key = Next#module_index.key,
    {ok, #template_file{
        filename=Next#module_index.filepath,
        template=Key#module_index_key.name
    }};
find_next_template(Filename, [_|Rest]) ->
    find_next_template(Filename, Rest).


% map_template_cat(Template, undefined, Context) ->
%     map_template_1(Template, Context);
% map_template_cat(Template, <<>>, Context) ->
%     map_template_1(Template, Context);
% map_template_cat(Template, [], Context) ->
%     map_template_1(Template, Context);
map_template_cat(Template, [Item|_]=IsA, Context) when is_atom(Item); is_binary(Item); is_list(Item) ->
    map_template_cat_1(Template, IsA, Context);
map_template_cat(Template, Id, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    case m_rsc:p_no_acl(Id, name, Context) of
        undefined ->
            map_template_cat_1(Template, IsA, Context);
        Name ->
            L = IsA ++ [ <<"name.", Name/binary>> ],
            map_template_cat_1(Template, L, Context)
    end.

map_template_cat_1(Template, Stack, Context) when is_binary(Template) ->
    Root = filename:rootname(Template),
    Ext = filename:extension(Template),
    case lists:foldr(
        fun
            (Cat, {error, enoent}) ->
                map_template_1(
                        <<Root/binary, $., (z_convert:to_binary(Cat))/binary, Ext/binary>>,
                        Context);
            (_Cat, Found) ->
                Found
         end,
         {error, enoent},
         Stack)
    of
        {error, enoent} -> map_template_1(Template, Context);
        {ok, _} = OK -> OK
    end.


%% @doc Dynamically find all templates matching the template
-spec map_template_all(template_compiler:template(), map(), term()) -> [template_compiler:template_file()].
map_template_all({cat, Template}, Vars, Context) ->
    case maps:get(id, Vars, undefined) of
        undefined -> map_template_all_1(Template, Context);
        Id -> map_template_all({cat, Template, Id}, Vars, Context)
    end;
map_template_all({cat, Template, [Item|_] = IsA}, _Vars, Context) when is_atom(Item); is_binary(Item); is_list(Item) ->
    map_template_all_cat(Template, IsA, Context);
map_template_all({cat, Template, Id}, _Vars, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> map_template_all_1(Template, Context);
        RscId -> map_template_all_cat(Template, RscId, Context)
    end;
map_template_all(Template, _Vars, Context) when is_binary(Template) ->
    map_template_all_1(Template, Context).

map_template_all_1(Template, Context) ->
    Tpls = z_module_indexer:find_all(template, Template, Context),
    [ #template_file{
                filename=Filename,
                template=Key#module_index_key.name
      } || #module_index{filepath=Filename, key=Key} <- Tpls ].

% map_template_all_cat(Template, undefined, Context)  ->
%     map_template_all_1(Template, Context);
% map_template_all_cat(Template, <<>>, Context) ->
%     map_template_all_1(Template, Context);
% map_template_all_cat(Template, [], Context) ->
%     map_template_all_1(Template, Context);
map_template_all_cat(Template, [Item|_]=IsA, Context) when is_atom(Item); is_binary(Item); is_list(Item) ->
    map_template_all_cat_1(Template, IsA, Context);
map_template_all_cat(Template, Id, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    case m_rsc:p_no_acl(Id, name, Context) of
        undefined ->
            map_template_all_cat_1(Template, IsA, Context);
        Name ->
            L = IsA ++ [ <<"name.", Name/binary>> ],
            map_template_all_cat_1(Template, L, Context)
    end.

map_template_all_cat_1(Template, Stack, Context) when is_binary(Template) ->
    Root = filename:rootname(Template),
    Ext = filename:extension(Template),
    Templates = lists:foldr(
        fun(Cat, Templates) ->
            Name = <<Root/binary, $., (z_convert:to_binary(Cat))/binary, Ext/binary>>,
            Templates ++ z_module_indexer:find_all(template, Name, Context)
        end,
        [],
        Stack),
    Tpls = Templates ++ z_module_indexer:find_all(template, Template, Context),
    [ #template_file{
                filename=Filename,
                template=Key#module_index_key.name
      } || #module_index{filepath=Filename, key=Key} <- Tpls ].


%% @doc Map a template name to a template file.
-spec map_template_1(binary(), z:context()) ->
            {ok, file:filename_all()} | {error, enoent|term()}.
map_template_1(Template, Context) when is_binary(Template) ->
    case z_module_indexer:find(template, Template, Context) of
        {ok, #module_index{filepath=Filename, key=Key}} ->
            {ok, #template_file{
                filename=Filename,
                template=Key#module_index_key.name
            }};
        {error, _} = Error ->
            Error
    end.


%% @doc Check if a file has been modified
-spec is_modified(file:filename_all(), calendar:datetime(), term()) -> boolean().
is_modified(Filename, Mtime, _Context) ->
    case z_file_mtime:mtime(Filename) of
        {ok, Mtime} -> false;
        {ok, _} -> true;
        {error, _} -> true
    end.


%% @doc Compile time mapping of nested value lookup
-spec compile_map_nested_value(Tokens :: list(), ContextVar :: string(), Context :: term()) -> NewTokens :: list().
compile_map_nested_value([{identifier, _, <<"q">>}, {identifier, _, <<"qargs">>}|Rest], ContextVar, _Context) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(get_qargs),
                [ erl_syntax:variable(ContextVar) ]),
    [{ast, Ast} | Rest];
compile_map_nested_value([{identifier, _, <<"q">>}, {identifier, _, QArg}|Rest], ContextVar, _Context) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(get_q),
                [ erl_syntax:abstract(QArg), erl_syntax:variable(ContextVar) ]),
    [{ast, Ast} | Rest];
compile_map_nested_value([{identifier, _, <<"q">>}, {expr, {string_literal, _, QArg}}|Rest], ContextVar, _Context) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(get_q),
                [ erl_syntax:abstract(QArg), erl_syntax:variable(ContextVar) ]),
    [{ast, Ast} | Rest];
compile_map_nested_value([{identifier, _, <<"z_language">>}], ContextVar, _Context) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(language),
                [ erl_syntax:variable(ContextVar) ]),
    [{ast, Ast}];
compile_map_nested_value([{identifier, _, <<"zotonic_site">>}], ContextVar, _Context) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(site),
                [ erl_syntax:variable(ContextVar) ]),
    [{ast, Ast}];
compile_map_nested_value([{identifier, _, <<"zotonic_dispatch">>}], ContextVar, _Context) ->
    get_z_context(zotonic_dispatch, ContextVar);
compile_map_nested_value([{identifier, _, <<"zotonic_dispatch_path">>}], ContextVar, _Context) ->
    get_z_context(zotonic_dispatch_path, ContextVar);
compile_map_nested_value([{identifier, _, <<"zotonic_dispatch_path_rewrite">>}], ContextVar, _Context) ->
    get_z_context(zotonic_dispatch_path_rewrite, ContextVar);
compile_map_nested_value([{identifier, _, <<"zotonic_version">>}], _ContextVar, _Context) ->
    [{ast, erl_syntax:abstract(z_convert:to_binary(?ZOTONIC_VERSION))}];
compile_map_nested_value(Ts, _ContextVar, _Context) ->
    Ts.

get_z_context(Var, ContextVar) ->
    Ast = erl_syntax:application(
                erl_syntax:atom(z_context),
                erl_syntax:atom(get),
                [ erl_syntax:atom(Var), erl_syntax:variable(ContextVar) ]),
    [{ast, Ast}].


%% @doc Find a list of values at once, easier and more efficient than a nested find_value/4
%%      Add pattern matching here for nested lookups.
find_nested_value([K|Ks], TplVars, Context) ->
    find_nested_value(find_value(K, TplVars, TplVars, Context), Ks, TplVars, Context).

%% @doc Find a list of values at once, easier and more efficient than a nested find_value/4
%%      Add pattern matching here for nested lookups.
find_nested_value(undefined, _Ks, _TplVars, _Context) ->
    undefined;
find_nested_value(V, [], _TplVars, _Context) ->
    V;
find_nested_value(V, [K|Ks], TplVars, Context) ->
    find_nested_value(find_value(K, V, TplVars, Context), Ks, TplVars, Context).


-define(is_property_key(K), (is_binary(K) orelse is_atom(K))).
-define(is_resource(K), (is_integer(K) orelse is_binary(K) orelse is_atom(K))).

%% @doc Find the value of key in some structure.
-spec find_value(Key :: term(), Vars :: term(), TplVars :: map(), Context :: term()) -> term().
find_value(undefined, _, _TplVars, _Context) ->
    undefined;
find_value(_, undefined, _TplVars, _Context) ->
    undefined;
find_value(Key, [N|_], _TplVars, Context) when ?is_property_key(Key), is_integer(N) ->
    % Assume a predicate/property lookup in a list of ids, map to lookup of first entry
    m_rsc:p(N, Key, Context);
find_value(Key, [{N}|_], _TplVars, Context) when ?is_property_key(Key), is_integer(N) ->
    % Assume a predicate/property lookup in a list of ids, map to lookup of first entry
    m_rsc:p(N, Key, Context);
find_value(Key, Id, _TplVars, Context) when ?is_property_key(Key), ?is_resource(Id) ->
    % Property of a resource, just assume an integer is a rsc id
    m_rsc:p(Id, Key, Context);
find_value(Key, {Id}, _TplVars, Context) when ?is_property_key(Key), ?is_resource(Id) ->
    % Property of a resource, just assume an integer is a rsc id
    m_rsc:p(Id, Key, Context);
find_value(Name, [[{A,_}|_]|_] = Blocks, _TplVars, _Context ) when is_atom(A), not is_integer(Name) ->
    % List of proplists - blocks in the old style list rsc
    NameB = z_convert:to_binary(Name),
    case lists:dropwhile(fun(Ps) -> proplists:get_value(name, Ps) =/= NameB end, Blocks) of
        [] -> undefined;
        [Block|_] -> Block
    end;
find_value(Nr, [{A,_}|_] = List, _TplVars, _Context ) when is_integer(Nr), is_integer(A) ->
    proplists:get_value(Nr, List);
find_value(Name, [#{ <<"name">> := _ }|_] = Blocks, _TplVars, _Context ) when not is_integer(Name) ->
    % List of maps - blocks in the rsc
    NameB = z_convert:to_binary(Name),
    case lists:dropwhile( fun (B) -> maps:get(<<"name">>, B, undefined) =/= NameB end, Blocks ) of
        [] -> undefined;
        [Block|_] -> Block
    end;
find_value(Key, #search_result{} = S, _TplVars, _Context) ->
    case Key of
        search -> {S#search_result.search_name, S#search_result.search_args};
        search_name -> S#search_result.search_name;
        search_args -> S#search_result.search_args;
        search_props -> S#search_result.search_args;
        result -> S#search_result.result;
        options -> S#search_result.options;
        total -> S#search_result.total;
        is_total_estimated -> S#search_result.total;
        page -> S#search_result.page;
        pages -> S#search_result.pages;
        next -> S#search_result.next;
        prev -> S#search_result.prev;
        facets -> S#search_result.facets;
        <<"search">> -> {S#search_result.search_name, S#search_result.search_args};
        <<"search_name">> -> S#search_result.search_name;
        <<"search_args">> -> S#search_result.search_args;
        <<"search_props">> -> S#search_result.search_args;
        <<"result">> -> S#search_result.result;
        <<"options">> -> S#search_result.options;
        <<"total">> -> S#search_result.total;
        <<"is_total_estimated">> -> S#search_result.is_total_estimated;
        <<"page">> -> S#search_result.page;
        <<"pages">> -> S#search_result.pages;
        <<"next">> -> S#search_result.next;
        <<"prev">> -> S#search_result.prev;
        <<"facets">> -> S#search_result.facets;
        Nth when is_integer(Nth) ->
            nth(Nth, S#search_result.result);
        Nth when is_binary(Nth) ->
            try
                Nth1 = z_convert:to_integer(Nth),
                nth(Nth1, S#search_result.result)
            catch
                _:_ -> undefined
            end
    end;
find_value(Key, #rsc_list{list=[H|_T] = L}, TplVars, Context) ->
    try
        case z_convert:to_integer(Key) of
            undefined -> find_value(Key, H, TplVars, Context);
            N -> nth(N, L)
        end
    catch
        error:badarg ->
            find_value(Key, H, TplVars, Context)
    end;
find_value(_Key, #rsc_list{list=[]}, _TplVars, _Context) ->
    undefined;
find_value(Key, #rsc_tree{ id = Id, tree = Tree }, TplVars, Context) ->
    case Key of
        id -> Id;
        tree -> Tree;
        <<"id">> -> Id;
        <<"tree">> -> Tree;
        Nth when is_integer(Nth) ->
            find_value(Nth, Tree, TplVars, Context);
        _ -> undefined
    end;
find_value(IsoAtom, Text, _TplVars, _Context) when is_atom(IsoAtom), is_binary(Text) ->
    case z_language:is_valid(atom_to_list(IsoAtom)) of
        true -> Text;
        false -> undefined
    end;
find_value(1, V, _TplVars, _Context) when is_binary(V); is_number(V); is_boolean(V) ->
    V;
find_value(N, V, _TplVars, _Context) when is_integer(N), (is_binary(V) orelse is_number(V) orelse is_boolean(V)) ->
    undefined;
find_value(1, #trans{} = V, _TplVars, _Context) ->
    V;
find_value(N, #trans{}, _TplVars, _Context) when is_integer(N) ->
    undefined;
find_value(1, #{ <<"@value">> := _ } = V, _TplVars, _Context) ->
    V;
find_value(N, #{ <<"@value">> := _ }, _TplVars, _Context) when is_integer(N) ->
    undefined;
find_value(1, #{ <<"@id">> := _ } = V, _TplVars, _Context) ->
    V;
find_value(N, #{ <<"@id">> := _ }, _TplVars, _Context) when is_integer(N) ->
    undefined;
find_value(<<"@id">>, #{ <<"@id">> := Uri  }, _TplVars, _Context) ->
    Uri;
find_value(K, #{ <<"@id">> := Uri }, TplVars, Context) when is_binary(K); is_atom(K) ->
    case m_rsc:rid(Uri, Context) of
        undefined -> undefined;
        RscId -> find_value(K, RscId, TplVars, Context)
    end;
find_value(K, [ #{} = Tuple | _ ], TplVars, Context) when is_binary(K) ->
    find_value(K, Tuple, TplVars, Context);
find_value(Key, Ps, TplVars, Context) ->
    template_compiler_runtime:find_value(Key, Ps, TplVars, Context).


nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 -> nth(N-1, T);
nth(_, _) -> undefined.


-spec get_context_name( term() ) -> atom().
get_context_name(Context) ->
    z_context:site(Context).

-spec set_context_vars(map()|list(), term()) -> term().
set_context_vars(Args, Context) when is_map(Args); is_list(Args) ->
    Lang = z_context:language(Context),
    Context1 = case get(z_language, Args, Lang) of
            undefined -> Context;
            Lang -> Context;
            NewLang -> z_context:set_language(NewLang, Context)
    end,
    Context2 = case z_convert:to_bool(get(sudo, Args, false)) of
            false -> Context1;
            true -> z_acl:sudo(Context1)
    end,
    Context3 = case z_convert:to_bool(get(anondo, Args, false)) of
            false -> Context2;
            true -> z_acl:anondo(Context2)
    end,
    Context4 = case get(extra_args, Args, undefined) of
                   #{ }=ExtraArgs ->
                       ExtraArgsProps = maps:to_list(ExtraArgs), 
                       ExtraArgsProps1 = [ {z_convert:to_atom(Key), Value} || {Key, Value} <- ExtraArgsProps ],
                       z_context:set(extra_args, ExtraArgsProps1, Context3);
                   _ -> Context3
               end,
    Context4.

get(Prop, Args, Default) when is_map(Args) ->
    maps:get(Prop, Args, Default);
get(Prop, Args, Default) when is_list(Args) ->
    proplists:get_value(Prop, Args, Default).


%% @doc Fetch the translations for the given text.
-spec get_translations(binary(), term()) -> binary() | z:trans().
get_translations(Text, Context) ->
    case z_trans:translations(Text, Context) of
        #trans{ tr = Tr } -> #trans{ tr = lists:sort(Tr) };
        Other -> Other
    end.

%% @doc Find the best fitting translation.
-spec lookup_translation(z:trans(), TplVars :: map(), Context :: term()) -> binary().
lookup_translation(#trans{} = Trans, _TplVars, Context) ->
    z_trans:lookup_fallback(Trans, Context).


%% @doc A model call with optional payload. Compiled from m.model.path::payload
-spec model_call(Model::atom(), Path::list(), Payload::term(), Context::term()) -> template_compiler:model_return().
model_call(Model, Path, Payload, Context) ->
    z_model:template_get(Model, Path, Payload, Context).

%% @doc Render a custom tag (Zotonic scomp)
%% @todo support render_optional/all
-spec custom_tag(Tag::atom(), Args::list(), Vars::map(), Context::term()) -> template_compiler:render_result().
custom_tag(Tag, Args, Vars, Context) ->
    z_scomp:render(Tag, Args, Vars, Context).


%% @doc Render image/image_url/media/url/lib tag. The Expr is the media item or dispatch rule.
-spec builtin_tag(template_compiler:builtin_tag(), Expr::term(), Args::list(), Vars::map(), Context::term()) ->
            template_compiler:render_result().
builtin_tag(Tag, Expr, Args, Vars, Context) ->
    builtin_tag_1(Tag, Expr, to_simple_values(Args, Context), Vars, Context).

builtin_tag_1(url, Dispatch, Args, _Vars, Context) ->
    z_dispatcher:url_for(z_convert:to_atom(Dispatch), Args, Context);
builtin_tag_1(lib, Libs, Args, _Vars, Context) ->
    z_lib_include:tag(Libs, Args, Context);
builtin_tag_1(lib_url, Libs, Args, _Vars, Context) ->
    z_lib_include:url(Libs, Args, Context);
builtin_tag_1(image, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_tag(Expr, Args, Context);
builtin_tag_1(image_url, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_url(Expr, Args, Context);
builtin_tag_1(image_data_url, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_data_url(Expr, Args, Context);
builtin_tag_1(media, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_viewer(Expr, Args, Context);
builtin_tag_1(Tag, _Expr, _Args, _Vars, _Context) ->
    ?LOG_WARNING(#{
        text => <<"Unknown template tag">>,
        in => zotonic_core,
        tag => Tag
    }),
    <<>>.


%% @doc Render a block, cache the result for some time. Caching should be implemented by the runtime.
%% @todo This needs to be updated for the modern ACL modules (see z_acl)
-spec cache_tag(MaxAge::integer(), Name::binary(), Args::list(), function(), TplVars::map(), Context::term()) ->
                template_compiler:render_result().
cache_tag(MaxAge, Name, Args, Fun, TplVars, Context) when is_integer(MaxAge) ->
    FunVars = lists:foldl(
                    fun({K,V}, Acc) ->
                        Acc#{K => V}
                    end,
                    TplVars,
                    Args),
    case do_cache(Args, Context) of
        false ->
            Fun(FunVars, Context);
        true ->
            Varies = lists:flatten(proplists:get_all_values(vary, Args)),
            Cat = proplists:get_all_values(cat, Args),
            Cat1 = lists:map(fun z_convert:to_atom/1, Cat),
            Key = {Name, Varies, z_acl:cache_key(Context)},
            F = fun() ->
                Fun(FunVars, Context)
            end,
            z_depcache:memo(F, Key, MaxAge, Varies ++ Cat1, Context)
    end;
cache_tag(undefined, Name, Args, Fun, TplVars, Context) ->
    case proplists:get_value(maxage, Args) of
        undefined -> cache_tag(1, Name, Args, Fun, TplVars, Context);
        MaxAge -> cache_tag(MaxAge, Name, Args, Fun, TplVars, Context)
    end;
cache_tag(MaxAge, Name, Args, Fun, TplVars, Context) ->
    cache_tag(z_convert:to_integer(MaxAge), Name, Args, Fun, TplVars, Context).

do_cache(Args, Context) ->
    case z_convert:to_bool( m_config:get_value(mod_development, nocache, Context) ) of
        true ->
            false;
        false ->
            do_cache1(z_convert:to_bool(proplists:get_value('if', Args, true)), Args, Context)
    end.

do_cache1(true, Args, Context) ->
    case z_convert:to_bool(proplists:get_value(if_anonymous, Args, false)) of
        true -> z_acl:user(Context) =:= undefined;
        false -> true
    end;
do_cache1(false, _Args, _Context) ->
    false.

%% @doc Render a script block, for Zotonic this is added to the scripts in the Context
-spec javascript_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().
javascript_tag(Block, _TplVars, Context) when is_binary(Block) ->
    z_context:get_render_state(z_render:wire({script, [{script, Block}]}, z_context:new(Context)));
javascript_tag(Block, _TplVars, Context) ->
    {Script, C} = z_render:render_to_iolist(Block, z_context:new(Context)),
    z_context:get_render_state(z_render:wire({script, [{script, iolist_to_binary(Script)}]}, C)).


%% @doc Remove spaces between HTML tags
-spec spaceless_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().
spaceless_tag(Value, TplVars, Context) ->
    template_compiler_runtime:spaceless_tag(Value, TplVars, Context).


%% @doc Convert a value to a boolean.
-spec to_bool(Value :: term(), Context :: term()) -> boolean().
to_bool(#trans{} = Tr, Context) ->
    case z_trans:lookup_fallback(Tr, Context) of
        undefined -> false;
        <<>> -> false;
        _ -> true
    end;
to_bool(#rsc_list{list=[]}, _Context) -> false;
to_bool(#rsc_list{list=[_|_]}, _Context) -> true;
to_bool(#search_result{result=[]}, _Context) -> false;
to_bool(#search_result{result=[_|_]}, _Context) -> true;
to_bool(null, _Context) -> false;
to_bool(Value, Context) ->
    z_convert:to_bool_strict( to_simple_value(Value, Context) ).

%% @doc Convert a value to a list.
-spec to_list(Value :: term(), Context :: term()) -> list().
to_list(undefined, _Context) -> [];
to_list(null, _Context) -> [];
to_list(<<>>, _Context) -> [];
to_list(#rsc_list{ list = L }, _Context) -> L;
to_list(#search_result{ result = L }, _Context) -> L;
to_list(q, Context) -> z_context:get_q_all(Context);
to_list(q_validated, _Context) -> [];
to_list(<<"q">>, Context) -> z_context:get_q_all(Context);
to_list(<<"q_validated">>, _Context) -> [];
to_list(#trans{} = Tr, _Context) -> [ Tr ];
to_list(V, _Context) when is_number(V); is_boolean(V); is_binary(V) -> [ V ];
to_list(#{ <<"@value">> := _ } = V, _Context) -> [ V ];
to_list(#{ <<"@id">> := _ } = V, _Context) -> [ V ];
to_list(V, Context) ->
    template_compiler_runtime:to_list(V, Context).


%% @doc Convert an argument list's values to something that can be handled as an argument to scomps.
-spec to_simple_values(Args::list(), Context::term()) -> list().
to_simple_values(Args, Context) when is_list(Args) ->
    [ {K, to_simple_value(Arg, Context)} || {K, Arg} <- Args ].

%% @doc Convert a value to something that can be handled as an argument to scomps.
-spec to_simple_value(Value::term(), Context::term()) -> term().
to_simple_value(#search_result{result=Result}, _Context) ->
    Result;
to_simple_value(#rsc_list{list=L}, _Context) ->
    L;
to_simple_value(#trans{} = Trans, Context) ->
    z_trans:lookup_fallback(Trans, Context);
to_simple_value(#{ <<"@value">> := Value } = V, _Context) ->
    case z_rdf_props:to_simple_value(V) of
        error -> Value;
        V1 -> V1
    end;
to_simple_value(#{ <<"@id">> := Uri }, _Context) ->
    Uri;
to_simple_value(V, _Context) ->
    V.


%% @doc Convert a value to an iolist, used for converting values in {{ ... }} expressions.
-spec to_render_result(Value::term(), TplVars:: map(), Context::term()) -> template_compiler:render_result().
to_render_result(undefined, _TplVars, _Context) ->
    <<>>;
to_render_result({{Y,M,D},{H,I,S}} = Date, TplVars, Context)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    try
        if
            M =:= 0 -> <<>>;
            D =:= 0 -> <<>>;
            true -> z_datetime:format(Date, "Y-m-d H:i:s", set_context_vars(TplVars, Context))
        end
    catch
        _:Error:Stack ->
            ?LOG_WARNING(#{
                in => zotonic_core,
                text => <<"Error formatting datetime tuple">>,
                result => error,
                reason => Error,
                stack => Stack,
                datetime => Date
            }),
            <<>>
    end;
to_render_result(#search_result{result=Result}, _TplVars, _Context) ->
    io_lib:format("~p", [Result]);
to_render_result(#rsc_list{list=L}, _TplVars, _Context) ->
    io_lib:format("~p", [L]);
to_render_result(#trans{} = Trans, TplVars, Context) ->
    z_trans:lookup_fallback(Trans, set_context_vars(TplVars, Context));
to_render_result(R, _TplVars, _Context) when element(1,R) =:= render_state ->
    % This is output from scomps or the filter show_media.
    R;
to_render_result(Vs, TplVars, Context) when is_list(Vs) ->
    % A list, which could be an unicode string but also a nested render result.
    % Assume that all integers are meant to be unicode characters.
    lists:map(
        fun
            ($<) -> <<"&lt;">>;
            ($>) -> <<"&gt;">>;
            (V) when is_integer(V), V > 0, V < 127 -> V;
            (V) when is_integer(V), V >= 128 ->
                try
                    <<V/utf8>>
                catch
                    error:badarg -> integer_to_binary(V)
                end;
            (V) when is_binary(V) -> V;
            (V) -> to_render_result(V, TplVars, Context)
        end,
        Vs);
to_render_result(#{ <<"@value">> := _ } = V, TplVars, Context) ->
    to_render_result(to_simple_value(V, Context), TplVars, Context);
to_render_result(#{ <<"@id">> := _ } = V, TplVars, Context) ->
    to_render_result(to_simple_value(V, Context), TplVars, Context);
to_render_result(V, TplVars, Context) ->
    template_compiler_runtime:to_render_result(V, TplVars, Context).


%% @doc HTML escape a value
-spec escape(Value :: iodata() | undefined, Context :: z:context()) -> iodata().
escape(undefined, _Context) ->
    <<>>;
escape(Value, _Context) ->
    z_html:escape(iolist_to_binary(Value)).

%% @doc Called when compiling a module
-spec trace_compile(atom(), binary(), template_compiler:options(), z:context()) -> ok.
trace_compile(Module, Filename, Options, Context) ->
    SrcPos = proplists:get_value(trace_position, Options),
    z_notifier:notify(
        #debug{
            what = template,
            arg = {compile, Filename, SrcPos, Module}
        }, Context),
    case SrcPos of
        {File, Line, _Col} ->
            ?LOG_DEBUG(#{
                text => <<"Template compile">>,
                in => zotonic_core,
                template => Filename,
                at => File,
                line => Line
            });
        undefined ->
            ?LOG_DEBUG(#{
                text => <<"Template compile">>,
                in => zotonic_core,
                template => Filename
            })
    end,
    ok.

%% @doc Called when a template is rendered (could be from an include). Optionally inserts
%% text before and after the text inclusion into the output stream.
-spec trace_render(TemplateFilename, Options, Context) -> ok | {ok, Before, After} when
    TemplateFilename :: binary(),
    Options :: template_compiler:options(),
    Context :: z:context(),
    Before :: iodata(),
    After :: iodata().
trace_render(Filename, Options, Context) ->
    SrcPos = proplists:get_value(trace_position, Options),
    z_notifier:notify_sync(
        #debug{
            what = template,
            arg = {render, Filename, SrcPos}
        }, Context),
    case m_config:get_boolean(mod_development, debug_includes, Context) of
        true ->
            case SrcPos of
                {File, 0, _Col} ->
                    ?LOG_NOTICE(#{
                        text => <<"Template extends/overrules">>,
                        in => zotonic_core,
                        template => Filename,
                        at => File
                    }),
                    {ok,
                        [ <<"\n<!-- START ">>, relpath(Filename),
                          <<" by ">>, relpath(File),
                          <<" -->\n">> ],
                        [ <<"\n<!-- END ">>, relpath(Filename),  <<" -->\n">> ]
                    };
                {File, Line, _Col} ->
                    ?LOG_NOTICE(#{
                        text => <<"Template include">>,
                        in => zotonic_core,
                        template => Filename,
                        at => File,
                        line => Line
                    }),
                    {ok,
                        [ <<"\n<!-- START ">>, relpath(Filename),
                          <<" by ">>, relpath(File), $:, integer_to_binary(Line),
                          <<" -->\n">> ],
                        [ <<"\n<!-- END ">>, relpath(Filename),  <<" -->\n">> ]
                    };
                undefined ->
                    ?LOG_NOTICE(#{
                        text => <<"Template render">>,
                        in => zotonic_core,
                        template => Filename
                    }),
                    {ok,
                        [ <<"\n<!-- START ">>, relpath(Filename), <<" -->\n">> ],
                        [ <<"\n<!-- END ">>, relpath(Filename),  <<" -->\n">> ]
                    }
            end;
        false ->
            ok
    end.


%% @doc Called when a block function is called
-spec trace_block({binary(), integer(), integer()}, atom(), atom(), term()) -> ok | {ok, iodata(), iodata()}.
trace_block({File, Line, _Col}, Name, Module, Context) ->
    case z_convert:to_bool(m_config:get_value(mod_development, debug_blocks, Context)) of
        true ->
            ?LOG_NOTICE(#{
                text => <<"Template call block">>,
                in => zotonic_core,
                block => Name,
                template => Module:filename(),
                at => File,
                line => Line
            }),
            {ok,
                [ <<"\n<!-- BLOCK ">>, atom_to_binary(Name, 'utf8'), " @ ", relpath(Module:filename()),
                  <<" by ">>, relpath(File), $:, integer_to_binary(Line),
                  <<" -->\n">> ],
                [ <<"\n<!-- ENDBLOCK ">>, atom_to_binary(Name, 'utf8'),  <<" -->\n">> ]
            };
        false ->
            ok
    end.


relpath(Filename) ->
    BaseLen = size(z_convert:to_binary(os:getenv("ZOTONIC"))),
    binary:part(Filename, BaseLen, size(Filename) - BaseLen).
