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
* templates: {prefix: optional, offset: 0, limit: 50}; indexed template names
  and winning source locations; limit is capped at 200.
* modules: active modules and their priorities.

Use /api/model/development_tools/get/<path> or
bridge/origin/model/development_tools/get/<path>. Results contain only JSON
values; Erlang tuples in dispatch metadata are represented as arrays.
Template names must be relative indexed names, not filesystem paths.

The CLI equivalent is bin/zotonic inspect SITE OPERATION [ARGUMENTS].
Supported operations: dispatch, dispatch-trace PATH, template NAME [RESOURCE],
templates [PREFIX], modules. No operation compiles, renders or deploys code.
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
get([<<"modules">>], _, Context) ->
    {ok, [#{module => M, priority => z_module_manager:prio(M)}
        || M <- z_module_manager:prio_sort(z_module_manager:active(Context))]};
get(_, _, _) -> {error, badarg}.

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
            {ok, #{requested => Requested, name => Name, filepath => File,
                candidates => [index(I) || I <- z_module_indexer:find_all(template, Name, Context)]}};
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
