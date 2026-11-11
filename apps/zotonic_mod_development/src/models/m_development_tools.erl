%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Administrator API for non-production dispatch and template inspection.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_development_tools).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "frontend_developer", "model",
        "development_and_debugging", "routing_and_redirects"
    ]
}).
-moduledoc("
Read-only development inspection. All endpoints require administrator and
mod_development access and an environment of development, test or acceptance.
Production is denied, including trusted SSH CLI calls.

GET paths (payload is supplied as query arguments over HTTP):

* dispatch/list: the site's active dispatch rules.
* dispatch/trace: {path: /local/path}; traces only this site, never selects a site by hostname.
* template/lookup: {name: page.tpl, id: optional resource id/name}; resolves the
  winning template and all candidates for its resolved name, in priority order.
  With id, uses the actual catinclude resolution rules. Does not render.
* template/source: same name and optional id as template/lookup, plus optional
  candidate (zero-based index in the resolved candidates list). Omitting candidate
  reads the winner. Returns lookup metadata plus source (UTF-8 text), candidate,
  module, priority and winner_filepath; filepath identifies the selected file.
  Follow overrules using the next candidate in the list. Each request resolves
  the list afresh; an absent candidate returns enoent, invalid selectors badarg.
  Reads only a candidate for that resolved name on this site, never a supplied path. Files larger than 1 MiB return too_large;
  invalid UTF-8 returns invalid_utf8. No rendering or compilation is performed.
* template/composition: same name/id/candidate selection as template/source.
  Returns selected metadata, inheritance (type none/extends/overrules, with a
  resolved target or error), local block names, and literal include metadata
  (template, line, column, method, is_catinclude). analysis is static;
  blocks_scope is local, includes_scope is literal_references. Compiles the
  selected indexed file if needed, without rendering or evaluating inheritance
  arguments. Does not recursively expand parents or resolve dynamic includes,
  include conditions, or category selections at include call sites. Missing
  parents appear as inheritance.error; compilation errors fail the request.
* templates: {prefix: optional, offset: 0, limit: 50}; indexed template names
  and winning source locations; limit is capped at 200.
* modules: active modules and their priorities.
* models: {prefix: optional, offset: 0, limit: 50}; paginated core and active-site
  models, effective Erlang module, source location and exported get/post/delete
  methods. Limit 1-200. Methods indicate callbacks, not permission to call them.
* model/documentation: {name: model name without m_}; returns Markdown module/API
  documentation, availability, methods, HTTP/MQTT topic patterns and static GET
  path hints. Documentation comes from BEAM docs with the existing source fallback.
  Missing documentation is explicitly reported; hints are incomplete (especially
  delegated/dynamic paths and POST/DELETE). A path segment any is a placeholder.
  No model callbacks are executed to discover data or schemas. Unknown/inactive
  models return enoent; arbitrary Erlang module names and paths are not accepted.

Use /api/model/development_tools/get/<path> or
bridge/origin/model/development_tools/get/<path>. Results contain only JSON
values; Erlang tuples in dispatch metadata are represented as arrays.
Template names must be relative indexed names, not filesystem paths.

Authorized non-production pages expose window.z_development_tools:
`await z_development_tools.dispatch()` lists rules;
`dispatchTrace(path)` traces a local path, defaulting to location.pathname;
`template(name, id)` resolves a template with optional resource category selection;
`templateSource(name, id, {candidate})` reads its source using the same resolution;
omit candidate for the winner, or select a zero-based override candidate;
`templateComposition(name, id, {candidate})` inspects static composition;
`templates({prefix, offset, limit})` lists templates; `modules()` lists modules;
`models({prefix, offset, limit})` discovers models; `modelDocumentation(name)`
reads their public API documentation and static path hints.
All return promises with the model result, rejecting errors with an error.code.
Inspection needs neither a capture attachment nor a visible debug button.

The CLI equivalent is bin/zotonic inspect SITE OPERATION [ARGUMENTS].
Supported operations: dispatch, dispatch-trace PATH, template NAME [RESOURCE],
templates [PREFIX], modules. These CLI operations do not compile, render or deploy code.
").
-behaviour(zotonic_model).
-export([m_get/3, allowed/1, cli/2]).
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").

-spec allowed(Context) -> boolean() when Context :: z:context().
allowed(Context) ->
    z_acl:is_admin(Context)
    andalso z_acl:is_allowed(use, mod_development, Context)
    andalso lists:member(m_site:environment(Context), [development, test, acceptance]).

m_get(Path, Msg, Context) ->
    case allowed(Context) of
        true ->
            Payload = case Msg of #{payload := P} when is_map(P) -> P; _ -> #{} end,
            case get(Path, Payload, Context) of
                {ok, Value} -> {ok, {json(Value), []}};
                {error, _} = Error -> Error
            end;
        false -> {error, eacces}
    end.

get([<<"dispatch">>, <<"list">>], _, Context) ->
    z:dispatch_list(Context);
get([<<"dispatch">>, <<"trace">>], #{<<"path">> := <<"/", _/binary>> = Path}, Context)
    when byte_size(Path) =< 4096 ->
    case Path of
        <<"//", _/binary>> -> {error, badarg};
        _ -> z:dispatch_path(Path, Context)
    end;
get([<<"template">>, Operation], #{<<"name">> := Name} = P, Context)
    when Operation =:= <<"source">>; Operation =:= <<"composition">> ->
    case valid_name(Name) of
        true ->
            case lookup(Name, maps:get(<<"id">>, P, undefined), Context) of
                {ok, Resolved} ->
                    case select_candidate(maps:get(<<"candidate">>, P, undefined), Resolved) of
                        {ok, Selected} -> inspect_template(Operation, Selected, Context);
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        false -> {error, badarg}
    end;
get([<<"template">>, <<"lookup">>], #{<<"name">> := Name} = P, Context) ->
    case valid_name(Name) of
        true -> lookup(Name, maps:get(<<"id">>, P, undefined), Context);
        false -> {error, badarg}
    end;
get([<<"templates">>], P, Context) ->
    Prefix = maps:get(<<"prefix">>, P, <<>>),
    Offset = number(maps:get(<<"offset">>, P, 0)),
    Limit = number(maps:get(<<"limit">>, P, 50)),
    case is_binary(Prefix) andalso byte_size(Prefix) =< 512
        andalso is_integer(Offset) andalso Offset >= 0
        andalso is_integer(Limit) andalso Limit > 0 andalso Limit =< 200 of
        true ->
            Names = lists:usort([N || #module_index{key = #module_index_key{name = N}}
                <- z_module_indexer:all(template, Context), is_binary(N),
                Prefix =:= <<>> orelse binary:match(N, Prefix) =:= {0, byte_size(Prefix)}]),
            Items = [index(I) || N <- Names,
                {ok, I} <- [z_module_indexer:find(template, N, Context)]],
            Page = lists:sublist(lists:nthtail(min(Offset, length(Items)), Items), Limit),
            {ok, #{items => Page, total => length(Items), offset => Offset, limit => Limit}};
        false -> {error, badarg}
    end;
get([<<"models">>], P, Context) ->
    Prefix = maps:get(<<"prefix">>, P, <<>>),
    Offset = number(maps:get(<<"offset">>, P, 0)),
    Limit = number(maps:get(<<"limit">>, P, 50)),
    case is_binary(Prefix) andalso byte_size(Prefix) =< 128
        andalso is_integer(Offset) andalso Offset >= 0
        andalso is_integer(Limit) andalso Limit > 0 andalso Limit =< 200 of
        true ->
            Models = [M || #{name := Name} = M <- site_models(Context),
                Prefix =:= <<>> orelse binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}],
            Page = lists:sublist(lists:nthtail(min(Offset, length(Models)), Models), Limit),
            {ok, #{items => [model_methods(M) || M <- Page],
                total => length(Models), offset => Offset, limit => Limit}};
        false -> {error, badarg}
    end;
get([<<"model">>, <<"documentation">>], #{<<"name">> := Name}, Context)
    when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 128 ->
    case [M || #{name := N} = M <- site_models(Context), N =:= Name] of
        [#{erlang_module := Module} = Model] ->
            case code:ensure_loaded(Module) of
                {module, Module} ->
                    Doc = z_development_lsp_docs:module(Module),
                    Hints = [H#{method => get} || H <- z_development_lsp_model:paths(Module)],
                    {ok, (model_methods(Model))#{
                        documentation => Doc,
                        documentation_format => <<"text/markdown">>,
                        documentation_available => Doc =/= <<>>,
                        path_hints => Hints,
                        path_hints_complete => false,
                        mqtt_topic => <<"bridge/origin/model/", Name/binary, "/{method}/{path}">>,
                        http_url => <<"/api/model/", Name/binary, "/{method}/{path}">>
                    }};
                {error, _} -> {error, unavailable}
            end;
        [] -> {error, enoent}
    end;
get([<<"modules">>], _, Context) ->
    {ok, [#{module => M, priority => z_module_manager:prio(M)}
        || M <- z_module_manager:prio_sort(z_module_manager:active(Context))]};
get(_, _, _) -> {error, badarg}.

%% Include core models and active apps, but honor z_model's effective resolution
%% (including its core-model precedence). Never turn client names into atoms.
site_models(Context) ->
    Indexed = z_module_indexer:all_files(model, zotonic_core)
        ++ z_module_indexer:all(model, Context),
    Names = lists:usort([z_convert:to_binary(N)
        || #module_index{key = #module_index_key{name = N}} <- Indexed]),
    [model_location(Name, Module, Indexed)
        || Name <- Names, {ok, Module} <- [z_model:get_module(Name, Context)]].

model_location(Name, Module, Indexed) ->
    Base = #{name => Name, erlang_module => Module},
    case [I || #module_index{erlang_module = M} = I <- Indexed, M =:= Module] of
        [#module_index{module = App, filepath = File} | _] ->
            Base#{module => App, filepath => File};
        [] -> Base
    end.

model_methods(#{erlang_module := Module} = Model) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Methods = [Verb || {Verb, Callback} <- [{get, m_get}, {post, m_post}, {delete, m_delete}],
                erlang:function_exported(Module, Callback, 3)],
            Model#{available => true, methods => Methods};
        {error, _} -> Model#{available => false, methods => []}
    end.

lookup(Name, undefined, Context) -> resolve(Name, Name, Context);
lookup(Name, Resource, Context) when is_binary(Resource); is_integer(Resource) ->
    case m_rsc:rid(Resource, Context) of
        undefined -> {error, enoent};
        Id -> resolve(Name, {cat, Name, Id}, Context)
    end;
lookup(_, _, _) -> {error, badarg}.

resolve(Requested, Template, Context) ->
    case z_template_compiler_runtime:map_template(Template, #{}, Context) of
        {ok, #template_file{filename = File, template = Name}} ->
            Matches = z_module_indexer:find_all(template, Name, Context),
            Candidates = [(index(I))#{candidate => N}
                || {N, I} <- lists:zip(lists:seq(0, length(Matches) - 1), Matches)],
            {ok, #{requested => Requested, name => Name, filepath => File,
                candidates => Candidates}};
        {error, _} = Error -> Error
    end.

%% Candidate selectors are positions in the freshly resolved site's index, never paths.
select_candidate(undefined, #{filepath := Winner, candidates := Candidates} = Resolved) ->
    case [C || #{filepath := File} = C <- Candidates, File =:= Winner] of
        [Selected | _] -> selected_candidate(Selected, Resolved);
        [] -> {error, enoent}
    end;
select_candidate(Value, #{candidates := Candidates} = Resolved) ->
    case number(Value) of
        N when is_integer(N), N >= 0 ->
            case [C || #{candidate := Position} = C <- Candidates, Position =:= N] of
                [Selected] -> selected_candidate(Selected, Resolved);
                [] -> {error, enoent}
            end;
        _ -> {error, badarg}
    end.

selected_candidate(Selected, #{filepath := Winner} = Resolved) ->
    {ok, (maps:merge(Resolved, Selected))#{winner_filepath => maps:get(winner_filepath, Resolved, Winner)}}.

inspect_template(<<"source">>, #{filepath := File} = Selected, _Context) ->
    case template_source(File) of
        {ok, Source} -> {ok, Selected#{source => Source}};
        {error, _} = Error -> Error
    end;
inspect_template(<<"composition">>, #{filepath := File, name := Name} = Selected, Context) ->
    %% Compile only the selected indexed file; never execute render or extends_runtime.
    Template = #template_file{filename = File, template = Name},
    case z_template:template_module(Template, #{}, Context) of
        {ok, Module} ->
            Includes = lists:sort(fun(A, B) ->
                {maps:get(line, A), maps:get(column, A)} =<
                    {maps:get(line, B), maps:get(column, B)}
            end, Module:includes()),
            {ok, Selected#{
                analysis => static,
                inheritance => inheritance(Module:extends(), Selected, Context),
                blocks => lists:sort(Module:blocks()),
                blocks_scope => local,
                includes => Includes,
                includes_scope => literal_references
            }};
        {error, _} = Error -> Error
    end.

inheritance(undefined, _Selected, _Context) -> #{type => none};
inheritance(overrules, #{candidate := N, name := Name} = Selected, _Context) ->
    inheritance_target(#{type => overrules, template => Name}, select_candidate(N + 1, Selected));
inheritance(Name, _Selected, Context) when is_binary(Name) ->
    Result = case valid_name(Name) of
        true -> lookup(Name, undefined, Context);
        false -> {error, badarg}
    end,
    inheritance_target(#{type => extends, template => Name}, Result).

inheritance_target(Relation, {ok, Target}) -> Relation#{target => Target};
inheritance_target(Relation, {error, Reason}) -> Relation#{error => Reason}.

%% Read only the server-resolved index path, with a bounded UTF-8 response.
-spec template_source(File) -> {ok, binary()} | {error, term()}
    when File :: file:filename_all().
template_source(File) ->
    case file:open(File, [read, binary, raw]) of
        {ok, Io} ->
            try
                case file:read(Io, 1048577) of
                    {ok, Data} when byte_size(Data) > 1048576 -> {error, too_large};
                    {ok, Data} ->
                        case unicode:characters_to_binary(Data, utf8, utf8) of
                            Source when is_binary(Source) -> {ok, Source};
                            _ -> {error, invalid_utf8}
                        end;
                    eof -> {ok, <<>>};
                    {error, _} = Error -> Error
                end
            after
                file:close(Io)
            end;
        {error, _} = Error -> Error
    end.

index(#module_index{key = #module_index_key{name = Name}, filepath = File, module = Module}) ->
    #{name => Name, filepath => File, module => Module, priority => z_module_manager:prio(Module)}.

valid_name(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 512 ->
    filename:pathtype(Name) =:= relative
        andalso not lists:member(<<"..">>, filename:split(Name))
        andalso binary:match(Name, <<0>>) =:= nomatch;
valid_name(_) -> false.

number(N) when is_integer(N) -> N;
number(B) when is_binary(B), byte_size(B) < 12 ->
    try binary_to_integer(B) catch error:badarg -> invalid end;
number(_) -> invalid.

%% Dispatch options can contain Erlang tuples, regex paths and proplists.
json(M) when is_map(M) -> maps:map(fun(_, V) -> json(V) end, M);
json(T) when is_tuple(T) -> [json(V) || V <- tuple_to_list(T)];
json([]) -> [];
json(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        false -> [json(V) || V <- L]
    end;
json(undefined) -> null;
json(V) when is_binary(V); is_atom(V); is_number(V) -> V;
json(V) -> iolist_to_binary(io_lib:format("~p", [V])).

-spec cli(SiteName, Args) -> {ok, term()} | {error, term()}
    when SiteName :: string(), Args :: [string()].
cli(SiteName, Args) ->
    case [S || S <- maps:keys(z_sites_manager:get_sites()),
        atom_to_binary(S, utf8) =:= unicode:characters_to_binary(SiteName)] of
        [Site] ->
            case arguments(Args) of
                {Path, Payload} ->
                    case m_get(Path, #{payload => Payload}, z_acl:sudo(z_context:new(Site))) of
                        {ok, {Result, []}} -> {ok, Result};
                        Error -> Error
                    end;
                error -> {error, badarg}
            end;
        [] -> {error, unknown_site}
    end.

arguments(["dispatch"]) -> {[<<"dispatch">>, <<"list">>], #{}};
arguments(["dispatch-trace", Path]) -> {[<<"dispatch">>, <<"trace">>], #{<<"path">> => unicode:characters_to_binary(Path)}};
arguments(["template", Name]) -> {[<<"template">>, <<"lookup">>], #{<<"name">> => unicode:characters_to_binary(Name)}};
arguments(["template", Name, Id]) -> {[<<"template">>, <<"lookup">>],
    #{<<"name">> => unicode:characters_to_binary(Name), <<"id">> => unicode:characters_to_binary(Id)}};
arguments(["templates"]) -> {[<<"templates">>], #{}};
arguments(["templates", Prefix]) -> {[<<"templates">>], #{<<"prefix">> => unicode:characters_to_binary(Prefix)}};
arguments(["modules"]) -> {[<<"modules">>], #{}};
arguments(_) -> error.
