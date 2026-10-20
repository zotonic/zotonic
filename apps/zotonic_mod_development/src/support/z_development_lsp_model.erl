%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Complete model paths from static Erlang clauses, declarations and selected-site predicates.
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

-module(z_development_lsp_model).
-export([complete/5, paths/1, infer/1]).

%% @doc Suggest path names without executing model callbacks or reading configuration values.
-spec complete(binary(), binary(), map(), boolean(), z:context()) -> [map()].
complete(Source, Prefix, #{model := Name, path := Path, part := Part} = Match, SiteKnown, Context) ->
    case z_model:get_module(Name, Context) of
        {ok, Module} ->
            Entries = paths(Module) ++ extra(Module, Path, SiteKnown, Context),
            Range = z_development_lsp_model_path:edit(Source, Prefix, Match),
            Candidates = lists:foldl(fun(Entry, Acc) ->
                case next(maps:get(path, Entry), Path) of
                    Key when is_binary(Key) ->
                        case starts_with(Key, Part) andalso safe_key(Key, maps:get(quote, Match)) of
                            true -> Acc#{Key => Entry};
                            false -> Acc
                        end;
                    _ ->
                        Acc
                end
            end, #{}, Entries),
            [
                #{
                    label => Key,
                    kind => 10,
                    detail => <<"Model ", Name/binary>>,
                    documentation => maps:get(description, Entry, <<>>),
                    textEdit => #{
                        range => Range,
                        newText => Key
                    }
                }
                || {Key, Entry} <- lists:sort(maps:to_list(Candidates))
            ];
        {error, _} ->
            []
    end.

starts_with(_, <<>>) -> true;
starts_with(Name, Part) -> binary:match(Name, Part) =:= {0, byte_size(Part)}.
safe_key(Key, dot) -> re:run(Key, <<"^[a-zA-Z_][a-zA-Z0-9_]*$">>, [{capture, none}]) =:= match;
safe_key(Key, Q) -> binary:match(Key, [<<Q>>, <<"\\">>, <<"\n">>, <<"\r">>]) =:= nomatch.

next([Key | _], []) -> Key;
next([any | Rest], [_ | Path]) -> next(Rest, Path);
next([Key | Rest], [Key | Path]) -> next(Rest, Path);
next(_, _) -> undefined.

%% @doc Cache static path descriptions by loaded module checksum; reloads replace the entry.
-spec paths(module()) -> [map()].
paths(Module) -> cached(Module, fun(Forms) -> infer(Forms) ++ metadata(Forms) end, paths).

cached(Module, Build, Kind) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Version = {Module:module_info(md5),
                proplists:get_value(model_paths, Module:module_info(attributes), [])},
            Key = {?MODULE, Kind, Module},
            case persistent_term:get(Key, undefined) of
                {Version, Value} ->
                    Value;
                _ ->
                    Value = Build(forms(Module)),
                    persistent_term:put(Key, {Version, Value}),
                    Value
            end;
        _ -> []
    end.

forms(Module) ->
    case code:get_object_code(Module) of
        {Module, Beam, _} ->
            case beam_lib:chunks(Beam, [abstract_code]) of
                {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} -> Forms;
                _ -> source_forms(Module)
            end;
        _ -> source_forms(Module)
    end.

source_forms(Module) ->
    case z_development_editor:source_location(Module) of
        {ok, File, _} ->
            case epp:parse_file(File, [], []) of
                {ok, Forms} -> Forms;
                _ -> []
            end;
        _ ->
            []
    end.

%% @doc Infer literal list elements and simple guarded alternatives in m_get/3 heads.
-spec infer([erl_parse:abstract_form()]) -> [map()].
infer(Forms) ->
    ModelFunctions = [Clauses || {function, _, m_get, 3, Clauses} <- Forms],
    Clauses = lists:append(ModelFunctions),
    lists:flatmap(fun infer_clause/1, Clauses).

%% @doc Extract path alternatives from a callback's first argument and its guards.
infer_clause({clause, _, [Head | _], Guards, _}) ->
    Paths = patterns(Head, Guards),
    [#{path => Path} || Path <- Paths, Path =/= []];
infer_clause(_) ->
    [].

patterns({match, _, Left, Right}, Guards) ->
    case patterns(Left, Guards) of [[]] -> patterns(Right, Guards); Paths -> Paths end;
patterns({cons, _, Head, Tail}, Guards) ->
    %% Bound combinations for unusual generated heads or guards.
    lists:sublist([[Part | Rest] || Part <- alternatives(Head, Guards), Rest <- patterns(Tail, Guards)], 128);
patterns(_, _) ->
    [[]].

alternatives({var, _, Name}, Guards) ->
    Values = lists:usort(lists:append([guard_values(G, Name) || G <- Guards])),
    case Values of
        [] -> [any];
        _ -> Values
    end;
alternatives(Form, _) ->
    case literal(Form) of
        undefined -> [any];
        Value -> [Value]
    end.

guard_values({op, _, Op, {var, _, Name}, Value}, Name) when Op =:= '=:='; Op =:= '==' ->
    case literal(Value) of undefined -> []; V -> [V] end;
guard_values({op, _, Op, Value, {var, _, Name}}, Name) when Op =:= '=:='; Op =:= '==' ->
    case literal(Value) of undefined -> []; V -> [V] end;
guard_values(List, Name) when is_list(List) -> lists:append([guard_values(V, Name) || V <- List]);
guard_values({op, _, Op, Left, Right}, Name) when Op =:= 'orelse'; Op =:= 'andalso' ->
    guard_values(Left, Name) ++ guard_values(Right, Name);
guard_values(_, _) ->
    [].

literal(Form) ->
    try erl_parse:normalise(Form) of
        B when is_binary(B) -> B;
        A when is_atom(A), A =/= undefined -> atom_to_binary(A, utf8);
        _ -> undefined
    catch
        _:_ -> undefined
    end.

%% @doc Optional model_paths attributes describe dynamic arguments (any) and returned fields.
metadata(Forms) ->
    Declarations = [Entries || {attribute, _, model_paths, Entries} <- Forms, is_list(Entries)],
    Entries = lists:append(Declarations),
    lists:filtermap(fun metadata_entry/1, Entries).

%% @doc Accept paths containing binary keys or argument placeholders, and normalize their description.
metadata_entry(#{path := Path} = Entry) when is_list(Path) ->
    IsValid = lists:all(fun(Part) -> is_binary(Part) orelse Part =:= any end, Path),
    case IsValid of
        true ->
            Description = maps:get(description, Entry, <<>>),
            {true, #{path => Path, description => unicode:characters_to_binary(Description)}};
        false ->
            false
    end;
metadata_entry(_) ->
    false.

%% @doc Collect literal keys from known dispatch helpers; do not call those helpers.
keys(Module, Functions) ->
    cached(Module, fun(Forms) ->
        lists:usort([Key || {function, _, F, Arity, Clauses} <- Forms,
            {Function, A, Position} <- Functions, F =:= Function, Arity =:= A,
            {clause, _, Args, _, _} <- Clauses,
            Key <- [literal(lists:nth(Position, Args))], is_binary(Key)])
    end, {keys, Functions}).

extra(m_rsc, [_], _, _) ->
    entries([any], lists:usort([<<"id">>, <<"uri">> | z_props:common_properties()] ++ keys(m_rsc, [{p_no_acl, 3, 2}])));
extra(m_req, [], _, _) ->
    entries([], keys(m_req, [{get, 2, 1}, {get_req, 2, 1}]));
extra(m_sysconfig, [], _, _) ->
    entries([], keys(z_config, [{get, 1, 1}, {default, 1, 1}]));
extra(m_config, [_, _], _, _) ->
    entries([any, any], [<<"value">>]);
extra(m_config, _, _, Context) ->
    config_paths(Context);
extra(m_predicate, Path, true, Context) ->
    case predicate_position(Path) of
        true -> predicate_entries(Path, Context);
        false -> []
    end;
extra(m_edge, [Operation, _Id] = Path, true, Context) ->
    case lists:member(Operation, [<<"o">>, <<"s">>, <<"o_props">>, <<"s_props">>, <<"id">>, <<"edges">>]) of
        true -> predicate_entries(Path, Context);
        false -> []
    end;
extra(_, _, _, _) ->
    [].

entries(Prefix, Names) -> [#{path => Prefix ++ [Name]} || Name <- Names].

%% @doc Suggest the site's predicate names without evaluating resource arguments or fetching edges.
predicate_entries(Path, Context) ->
    Names = [atom_to_binary(Name, utf8) || {Name, _} <- m_predicate:all(Context),
        is_atom(Name), Name =/= undefined],
    entries(Path, Names).

predicate_position([]) -> true;
predicate_position([Name]) -> lists:member(Name, [<<"predicate">>, <<"is_used">>,
    <<"object_resources">>, <<"object_category">>, <<"subject_category">>,
    <<"is_valid_object_in_category">>, <<"is_valid_object_subcategory">>,
    <<"is_valid_object_category">>, <<"is_valid_subject_subcategory">>, <<"is_valid_subject_category">>]);
predicate_position(_) -> false.

%% @doc Configuration declarations expose names and descriptions, never configured values.
config_paths(Context) ->
    %% m.config.zotonic exposes the system keys handled by z_config.
    SystemKeys = keys(z_config, [{get, 1, 1}, {default, 1, 1}]),
    SystemPaths = entries([<<"zotonic">>], SystemKeys),

    %% Collect declarations from the core and each active application.
    Apps = [zotonic_core | z_module_manager:active(Context)],
    Declarations = lists:append([z_module_manager:mod_config(App) || App <- Apps]),
    ModulePaths = [config_path(Config) || #{module := _, key := _} = Config <- Declarations],
    SystemPaths ++ ModulePaths.

%% @doc Use the declared configuration namespace, which can differ from its owning application.
config_path(#{module := Module, key := Key} = Config) ->
    #{
        path => [z_convert:to_binary(Module), z_convert:to_binary(Key)],
        description => unicode:characters_to_binary(maps:get(description, Config, <<>>))
    }.
