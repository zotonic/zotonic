%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Simple runtime for the compiled templates. Needs to be
%%      copied and adapted for different environments.

%% Copyright 2016 Marc Worrell
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
    set_context_vars/2,
    get_translations/2,
    lookup_translation/3,
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

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_release.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").


%% @doc Dynamic mapping of a template to a template name, context sensitive on the template vars.
-spec map_template(template_compiler:template(), map(), term()) ->
            {ok, template_compiler:template_file()} | {error, enoent|term()}.
map_template(#template_file{} = Tpl, _Vars, _Context) ->
    {ok, Tpl};
map_template({cat, Template}, Vars, Context) ->
    case maps:get(id, Vars, undefined) of
        undefined -> map_template_1(Template, Context);
        Id -> map_template({cat, Template, Id}, Vars, Context)
    end;
map_template({cat, Template, [Cat|_] = IsA}, _Vars, Context) when is_atom(Cat) ->
    map_template_cat(Template, IsA, Context);
map_template({cat, Template, Id}, _Vars, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> map_template_1(Template, Context);
        RscId -> map_template_cat(Template, RscId, Context)
    end;
map_template({overrules, Template, Filename}, _Vars, Context) ->
    Templates = z_module_indexer:find_all(template, Template, Context),
    find_next_template(Filename, Templates);
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


map_template_cat(Template, None, Context) when None =:= <<>>; None =:= undefined; None =:= [] ->
    map_template_1(Template, Context);
map_template_cat(Template, [Item|_]=IsA, Context) when is_atom(Item) ->
    map_template_cat_1(Template, IsA, Context);
map_template_cat(Template, Id, Context) ->
    IsA = case {m_rsc:is_a(Id, Context), m_rsc:p(Id, name, Context)} of
                {L, undefined} -> L;
                {L, Name} -> L ++ [Name]
          end,
    map_template_cat_1(Template, IsA, Context).

map_template_cat_1(Template, Stack, Context) when is_binary(Template) ->
    Root = filename:rootname(Template),
    Ext = filename:extension(Template),
    case lists:foldr(fun(Cat, {error, enoent}) ->
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
-spec map_template_all(template_compiler:template(), map(), term()) -> [template_compiler:template1()].
map_template_all({cat, Template}, Vars, Context) ->
    case maps:get(id, Vars, undefined) of
        undefined -> map_template_all_1(Template, Context);
        Id -> map_template_all({cat, Template, Id}, Vars, Context)
    end;
map_template_all({cat, Template, [Cat|_] = IsA}, _Vars, Context) when is_atom(Cat) ->
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

map_template_all_cat(Template, None, Context) when None =:= <<>>; None =:= undefined; None =:= [] ->
    map_template_all_1(Template, Context);
map_template_all_cat(Template, [Item|_]=IsA, Context) when is_atom(Item) ->
    map_template_all_cat_1(Template, IsA, Context);
map_template_all_cat(Template, Id, Context) ->
    IsA = case {m_rsc:is_a(Id, Context), m_rsc:p(Id, name, Context)} of
                {L, undefined} -> L;
                {L, Name} -> L ++ [Name]
          end,
    map_template_all_cat_1(Template, IsA, Context).

map_template_all_cat_1(Template, Stack, Context) when is_binary(Template) ->
    Root = filename:rootname(Template),
    Ext = filename:extension(Template),
    Templates = lists:foldr(fun(Cat, Templates) ->
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
-spec map_template_1(binary(), #context{}) ->
            {ok, filename:filename()} | {error, enoent|term()}.
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
%% @todo Profile this and consider the memo cache to speed this up
-spec is_modified(filename:filename(), calendar:datetime(), term()) -> boolean().
is_modified(Filename, Mtime, _Context) ->
    case z_file_mtime:mtime(Filename) of
        {ok, Mtime} -> false;
        {ok, _} -> true;
        {error, _} -> true
    end.


%% @doc Compile time mapping of nested value lookup
-spec compile_map_nested_value(Tokens :: list(), ContextVar :: string(), Context :: term()) -> NewTokens :: list().
compile_map_nested_value([{identifier, _, <<"m">>}, {identifier, _, Model}|Rest], _ContextVar, _Context) ->
    Module = binary_to_atom(<<"m_", Model/binary>>, 'utf8'),
    [{mfa, Module, m_get, Rest}];
compile_map_nested_value([{identifier, _, <<"q">>}, {identifier, _, QArg}|Rest], ContextVar, _Context) ->
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


%% @doc Find the value of key in some structure.
-spec find_value(Key :: term(), Vars :: term(), TplVars :: map(), Context :: term()) -> term().
find_value(undefined, _, _TplVars, _Context) ->
    undefined;
find_value(_, undefined, _TplVars, _Context) ->
    undefined;
find_value(Key, #search_result{} = S, _TplVars, _Context) when is_integer(Key) ->
    try
        lists:nth(Key, S#search_result.result)
    catch
        _:_ -> undefined
    end;
find_value(Key, [N|_], _TplVars, Context) when is_atom(Key), is_integer(N) ->
    % Assume a predicate/property lookup in a list of ids, map to lookup of first entry
    m_rsc:p(N, Key, Context);
find_value(Key, Id, _TplVars, Context) when is_atom(Key), is_integer(Id) ->
    % Property of a resource, just assume an integer is a rsc id
    m_rsc:p(Id, Key, Context);
find_value(Key, RscName, _TplVars, Context) when is_atom(Key), is_atom(RscName) ->
    % Property of a resource, just assume an integer is a rsc id
    m_rsc:p(RscName, Key, Context);
find_value(Name, [[{A,_}|_]|_] = Blocks, _TplVars, _Context ) when is_atom(A), not is_integer(Name) ->
    % List of proplists - blocks in the rsc
    NameB = z_convert:to_binary(Name),
    case lists:dropwhile(fun(Ps) -> proplists:get_value(name, Ps) =/= NameB end, Blocks) of
        [] -> undefined;
        [Block|_] -> Block
    end;
find_value(Key, #m_search_result{} = S, TplVars, Context) when is_integer(Key) ->
    find_value(Key, S#m_search_result.result, TplVars, Context);
find_value(Key, #search_result{} = S, _TplVars, _Context) when is_atom(Key) ->
    case Key of
        result -> S#search_result.result;
        all -> S#search_result.all;
        total -> S#search_result.total;
        page -> S#search_result.page;
        pages -> S#search_result.pages;
        next -> S#search_result.next;
        prev -> S#search_result.prev
    end;
find_value(Key, #m_search_result{} = S, _TplVars, _Context) when is_atom(Key) ->
    case Key of
        search -> {S#m_search_result.search_name, S#m_search_result.search_props};
        search_name -> S#m_search_result.search_name;
        search_props -> S#m_search_result.search_props;
        result -> S#m_search_result.result;
        total -> S#m_search_result.total;
        page -> S#m_search_result.page;
        pages -> S#m_search_result.pages;
        pagelen -> S#m_search_result.pagelen;
        next -> S#m_search_result.next;
        prev -> S#m_search_result.prev
    end;
find_value(Key, #rsc_list{list=L}, _TplVars, _Context) when is_integer(Key) ->
    try lists:nth(Key, L)
    catch _:_ -> undefined
    end;
find_value(Key, #rsc_list{list=[H|_T]}, TplVars, Context) ->
    find_value(Key, H, TplVars, Context);
find_value(_Key, #rsc_list{list=[]}, _TplVars, _Context) ->
    undefined;
find_value(IsoAtom, Text, _TplVars, _Context) when is_atom(IsoAtom), is_binary(Text) ->
    case z_language:is_valid(atom_to_list(IsoAtom)) of
        true -> Text;
        false -> undefined
    end;
find_value(Key, Ps, TplVars, Context) ->
    template_compiler_runtime:find_value(Key, Ps, TplVars, Context).


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
    Context3.

get(Prop, Args, Default) when is_map(Args) ->
    maps:get(Prop, Args, Default);
get(Prop, Args, Default) when is_list(Args) ->
    proplists:get_value(Prop, Args, Default).


%% @doc Fetch the translations for the given text.
-spec get_translations(binary(), term()) -> binary() | {trans, [{atom(), binary()}]}.
get_translations(Text, Context) ->
    case z_trans:translations(Text, Context) of
        {trans, Tr} -> {trans, lists:sort(Tr)};
        Other -> Other
    end.

%% @doc Find the best fitting translation.
-spec lookup_translation({trans, list({atom(), binary()})}, TplVars :: map(), Context :: term()) -> binary().
lookup_translation({trans, _} = Trans, _TplVars, Context) ->
    z_trans:lookup_fallback(Trans, Context).


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
builtin_tag_1(image, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_tag(Expr, Args, Context);
builtin_tag_1(image_url, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_url(Expr, Args, Context);
builtin_tag_1(media, Expr, Args, _Vars, Context) ->
    z_media_tag:scomp_viewer(Expr, Args, Context);
builtin_tag_1(Tag, _Expr, _Args, _Vars, Context) ->
    lager:info("[~p] Unknown tag ~p", [z_context:site(Context), Tag]),
    <<>>.


%% @doc Render a block, cache the result for some time. Caching should be implemented by the runtime.
%% @todo This needs to be updated for the modern ACL modules (see z_acl)
-spec cache_tag(MaxAge::integer(), Name::binary(), Args::list(), function(), TplVars::map(), Context::term()) ->
                template_compiler:render_result().
cache_tag(MaxAge, Name, Args, Fun, TplVars, Context) ->
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
    end.

do_cache(Args, Context) ->
    do_cache1(z_convert:to_bool(proplists:get_value('if', Args, true)), Args, Context).

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
to_bool({trans, _} = Tr, Context) ->
    case z_trans:lookup_fallback(Tr, Context) of
        undefined -> false;
        <<>> -> false;
        _ -> true
    end;
to_bool(#search_result{result=L}, Context) ->
    to_bool(L, Context);
to_bool(#m_search_result{result=L}, Context) ->
    to_bool(L, Context);
to_bool(Value, _Context) ->
    z_convert:to_bool_strict(Value).

%% @doc Convert a value to a list.
-spec to_list(Value :: term(), Context :: term()) -> list().
to_list(undefined, _Context) -> [];
to_list(#rsc_list{list=L}, _Context) -> L;
to_list(#search_result{result=L}, _Context) -> L;
to_list(#m_search_result{result=Result}, Context) -> to_list(Result, Context);
to_list(q, Context) -> z_context:get_q_all(Context);
to_list(q_validated, _Context) -> [];
to_list({trans, _}, _Context) -> [];
to_list(V, Context) -> template_compiler_runtime:to_list(V, Context).


%% @doc Convert an argument list's values to something that can be handled as an argument to scomps.
-spec to_simple_values(Args::list(), Context::term()) -> list().
to_simple_values(Args, Context) when is_list(Args) ->
    [ {K, to_simple_value(Arg, Context)} || {K, Arg} <- Args ].

%% @doc Convert a value to something that can be handled as an argument to scomps.
-spec to_simple_value(Value::term(), Context::term()) -> term().
to_simple_value(#m_search_result{result=#search_result{result=Result}}, _Context) ->
    Result;
to_simple_value(#rsc_list{list=L}, _Context) ->
    L;
to_simple_value({trans, _} = Trans, Context) ->
    z_trans:lookup_fallback(Trans, Context);
to_simple_value(V, _Context) ->
    V.


%% @doc Convert a value to an iolist, used for converting values in {{ ... }} expressions.
-spec to_render_result(Value::term(), TplVars:: map(), Context::term()) -> template_compiler:render_result().
to_render_result(undefined, _TplVars, _Context) ->
    <<>>;
to_render_result({{Y,M,D},{H,I,S}} = Date, TplVars, Context)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    z_datetime:format(Date, "Y-m-d H:i:s", set_context_vars(TplVars, Context));
to_render_result(#m_search_result{result=#search_result{result=Result}}, _TplVars, _Context) ->
    io_lib:format("~p", [Result]);
to_render_result(#rsc_list{list=L}, _TplVars, _Context) ->
    io_lib:format("~p", [L]);
to_render_result({trans, _} = Trans, TplVars, Context) ->
    z_trans:lookup_fallback(Trans, set_context_vars(TplVars, Context));
to_render_result(V, TplVars, Context) ->
    template_compiler_runtime:to_render_result(V, TplVars, Context).


%% @doc HTML escape a value
-spec escape(Value :: iolist(), Context :: term()) -> iolist().
escape(Value, _Context) ->
    z_html:escape(iolist_to_binary(Value)).

%% @doc Called when compiling a module
-spec trace_compile(atom(), binary(), template_compiler:options(), term()) -> ok.
trace_compile(Module, Filename, Options, Context) ->
    SrcPos = proplists:get_value(trace_position, Options),
    z_notifier:notify(
        #debug{
            what=template,
            arg={compile, Filename, SrcPos, Module}
        }, Context),
    case SrcPos of
        {File, Line, _Col} ->
            lager:debug("[~p] Compiling \"~s\" (called from \"~s:~p\")",
                        [z_context:site(Context), Filename, File, Line]);
        undefined ->
            lager:debug("[~p] Compiling \"~s\"",
                        [z_context:site(Context), Filename])
    end.

%% @doc Called when a template is rendered (could be from an include)
-spec trace_render(binary(), template_compiler:options(), term()) -> ok.
trace_render(Filename, Options, Context) ->
    SrcPos = proplists:get_value(trace_position, Options),
    case z_convert:to_bool(m_config:get_value(mod_development, debug_includes, Context)) of
        true ->
            case SrcPos of
                {File, Line, _Col} ->
                    lager:info("[~p] Include \"~s\" by \"~s:~p\"",
                               [z_context:site(Context), Filename, File, Line]),
                    {ok,
                        [ <<"\n<!-- START ">>, relpath(Filename),
                          <<" by ">>, relpath(File), $:, integer_to_binary(Line),
                          <<" -->\n">> ],
                        [ <<"\n<!-- END ">>, relpath(Filename),  <<" -->\n">> ]
                    };
                undefined ->
                    lager:info("[~p] Render \"~s\"",
                               [z_context:site(Context), Filename]),
                    {ok,
                        [ <<"\n<!-- START ">>, relpath(Filename), <<" -->\n">> ],
                        [ <<"\n<!-- END ">>, relpath(Filename),  <<" -->\n">> ]
                    }
            end;
        false ->
            ok
    end.

%% @doc Called when a block function is called
-spec trace_block({binary(), integer(), integer()}, atom(), atom(), term()) -> ok | {ok, iolist(), iolist()}.
trace_block({File, Line, _Col}, Name, Module, Context) ->
    case z_convert:to_bool(m_config:get_value(mod_development, debug_blocks, Context)) of
        true ->
            lager:info("[~p] Call block \"~p\" in \"~s\" by \"~s:~p\"",
                       [z_context:site(Context), Name, Module:filename(), File, Line]),
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
