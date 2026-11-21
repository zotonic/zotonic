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
Inspect a running site's dispatch rules, template resolution, source, composition,
and available model APIs. Combine this model with `model#development_capture`
when conditional or lazy rendering needs runtime evidence.

For local and remote workflows, see the
[LLM site development guide](../../../../doc/guides/llm-site-development.md).

## Access

All endpoints require an administrator with `use mod_development` permission
and a site environment of `development`, `test`, or `acceptance`. Production
is denied, including trusted CLI calls. Read-only administrators can use these
inspection endpoints.

Inspection does not render pages, edit source, or deploy changes. Template
composition may compile an indexed template to obtain its metadata.

## Browser quick start

Authorized pages with the development head assets expose
`window.z_development_tools`. Inspection requires neither a capture attachment
nor a visible debug button.

```javascript
const tools = window.z_development_tools;
const route = await tools.dispatchTrace();
const template = await tools.template('page.tpl', 'article_resource_name');
const source = await tools.templateSource('page.tpl', 'article_resource_name');
const composition = await tools.templateComposition('page.tpl', 'article_resource_name');
```

All methods return promises containing the model result. Model errors reject
with `error.code`; transport failures also reject.

| JavaScript method | Result |
| --- | --- |
| `dispatch()` | Active dispatch rules. |
| `dispatchTrace(path?)` | Dispatch trace for a local path; defaults to `location.pathname`. |
| `template(name, id?)` | Winning template and indexed override candidates. |
| `templateSource(name, id?, {candidate}?)` | Selected candidate's metadata and UTF-8 source. |
| `templateComposition(name, id?, {candidate}?)` | Selected candidate's inheritance, local blocks, and literal includes. |
| `templates({prefix, offset, limit}?)` | Paginated indexed template names and winning locations. |
| `modules()` | Active modules and priorities. |
| `models({prefix, offset, limit}?)` | Paginated available models and exported callback methods. |
| `modelDocumentation(name)` | Model documentation, availability, transport patterns, and incomplete GET-path hints. |

## Template selection and source

Use relative indexed template names, never filesystem paths. The optional `id`
is a resource ID or name and applies the same category selection as `catinclude`.
Lookup returns the resolved `name`, winning `filepath`, and `candidates` in module
priority order. Lower numeric priorities win.

Source and composition accept a zero-based `candidate` position from that list.
Omit it for the winner; pass `undefined` for the resource when selecting a candidate
without category lookup:

```javascript
const current = await tools.templateSource('page.tpl');
const next = current.candidates.find(c => c.candidate === current.candidate + 1);
if (next) {
    const parent = await tools.templateSource(current.name, undefined, {
        candidate: next.candidate
    });
}
```

Use the next candidate to follow `{% overrules %}`. Each request resolves the
index afresh. Source results include `source`, `candidate`, `module`, `priority`,
and `winner_filepath`; `filepath` identifies the file actually read. Source
reading performs no rendering or compilation and is limited to 1 MiB of UTF-8.

## Static composition

Composition uses compiler metadata and may compile the selected file. It does
not render templates or evaluate runtime inheritance arguments.

| Field | Meaning |
| --- | --- |
| `inheritance` | `type` is `none`, `extends`, or `overrules`; a resolved parent is in `target`, or lookup failure in `error`. |
| `blocks` | Names defined in this file, not inherited or effective blocks. |
| `includes` | Literal references with `template`, `line`, `column`, `method`, and `is_catinclude`. |
| `analysis` | `static`. |
| `blocks_scope` | `local`. |
| `includes_scope` | `literal_references`. |

Include methods are `normal`, `optional`, or `all`. Dynamic include names,
conditional choices, and resource category selection at include call sites are
not resolved. Follow parent targets and include names with further lookups;
track visited files to avoid cycles. A missing parent is reported in
`inheritance.error`; compilation errors fail the request.

## Discover model APIs

```javascript
const models = await tools.models({prefix: 'rsc', offset: 0, limit: 50});
const api = await tools.modelDocumentation('rsc');
```

Model names omit `m_`. Discovery includes core and active-site models, honoring
the site's effective resolution and core-model precedence. Entries identify
`name`, `erlang_module`, owning `module`, `filepath` when available, load
availability, and exported `get`, `post`, or `delete` callback methods.

Documentation comes from BEAM metadata with a source fallback. The result
includes Markdown `documentation`, `documentation_available`, `methods`,
`mqtt_topic`, `http_url`, and `path_hints`. No model callbacks are invoked to
probe supported arguments or schemas.

`path_hints_complete` is always false. Hints can omit delegated or dynamic GET
paths and do not describe POST/DELETE schemas. A segment named `any` is a
placeholder. Exported callbacks and path hints do not grant permission to call
them. Unknown or inactive model names cannot be used to inspect arbitrary Erlang
modules or files.

## HTTP and MQTT reference

All operations use GET. HTTP paths start with
`/api/model/development_tools/get/`; supply arguments as query fields. Browser
MQTT topics start with `bridge/origin/model/development_tools/get/`; supply
arguments as the payload object.

| Path | Arguments |
| --- | --- |
| `dispatch/list` | None. |
| `dispatch/trace` | `path`: local absolute URL path, including any language prefix. |
| `template/lookup` | `name`, optional resource `id`. |
| `template/source` | `name`, optional `id` and `candidate`. |
| `template/composition` | Same selection as source. |
| `templates` | Optional `prefix`, `offset`, `limit`. |
| `modules` | None. |
| `models` | Optional `prefix`, `offset`, `limit`. |
| `model/documentation` | `name` without `m_`. |

Pagination defaults to an empty prefix, offset 0, and limit 50. Limits range
from 1 to 200; offsets must be nonnegative. Dispatch tracing stays within the
selected site and does not render its controller. Erlang tuples in dispatch
results become JSON arrays.

HTTP JSON and MQTT response payloads use `{status: \"ok\", result: ...}` or
`{status: \"error\", error: ...}`. Common errors are:

| Error | Meaning |
| --- | --- |
| `eacces` | Missing administrator/module access or unsupported environment. |
| `badarg` | Invalid path, template name, selector, or pagination. |
| `enoent` | Missing resource, template, candidate, source file, or model. |
| `too_large` | Template source exceeds 1 MiB. |
| `invalid_utf8` | Template source is not valid UTF-8. |
| `unavailable` | A discovered model implementation cannot be loaded. |

## CLI

Run from the server checkout, locally or through SSH:

```sh
bin/zotonic inspect mysite dispatch
bin/zotonic inspect mysite dispatch-trace /nl/news/example
bin/zotonic inspect mysite template page.tpl article_resource_name
bin/zotonic inspect mysite templates _article
bin/zotonic inspect mysite modules
```

These CLI operations do not compile or render templates. Source, composition,
and model documentation are available through JavaScript, HTTP, and MQTT.
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
