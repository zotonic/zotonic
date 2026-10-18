%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Resolve template diagnostics, completions and source references using running development sites.
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

-module(z_development_lsp_backend).

-moduledoc(<<"
Read-only template intelligence for trusted local development tools. Unsaved text is scanned and
parsed, never compiled or rendered. Site names are matched against running development sites
without creating atoms.
">>).

-export([request/2, sites/0, diagnostics/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_SOURCE_BYTES, (4 * 1024 * 1024)).

%% @doc List running sites with mod_development enabled, for editor site selectors.
-spec sites() -> [binary()].
sites() ->
    lists:sort([
        atom_to_binary(S, utf8)
        ||  {S, running} <- maps:to_list(z_sites_manager:get_sites()),
            lists:member(mod_development, z_module_manager:active(z_context:new(S)))
    ]).

%% @doc Resolve a bounded editor request in a live site's module-priority context.
-spec request(atom(), map()) -> term().
request(sites, _) ->
    sites();
request(diagnostics, #{source := Source}) ->
    diagnostics(Source);
request(Kind, #{source := Source, file := File} = Args0) when byte_size(Source) =< ?MAX_SOURCE_BYTES ->
    Lines = z_development_lsp_text:lines(Source),
    Ref = case Kind of
        completion -> undefined;
        _ -> reference_at_cursor(Source, Lines, maps:get(position, Args0))
    end,
    Args = Args0#{lines => Lines, reference => Ref},
    Available = sites(),
    case maps:get(site, Args, <<>>) of
        <<>> ->
            case tag_hover(Kind, Args) of
                undefined when Ref =:= undefined, Kind =:= definition -> [];
                undefined when Ref =:= undefined, Kind =:= hover -> null;
                undefined ->
                    Contexts = [site_context(S) || S <- Available],
                    Owners = case Kind =/= completion andalso server_reference(Args) of
                        true -> Contexts;
                        false -> [C || C <- Contexts, contains_file(File, C)]
                    end,
                    Candidates = case Owners of [] -> Contexts; _ -> Owners end,
                    automatic_query(Kind, Args, Candidates, Contexts);
                Hover -> Hover
            end;
        Selected ->
            case lists:member(Selected, Available) of
                true ->
                    case tag_hover(Kind, Args) of
                        undefined -> query(Kind, Args, site_context(Selected));
                        Hover -> Hover
                    end;
                false ->
                    {error, <<"Selected site is not running with mod_development enabled.">>}
            end
    end.

%% @doc Tag manuals are independent of site resolution and do not imply a definition target.
tag_hover(hover, #{source := Source, position := Position} = Args) ->
    case template_compiler_scanner:scan(<<"buffer">>, Source) of
        {ok, Tokens} -> tag_hover_tokens(Tokens, maps:get(lines, Args), Position, Args);
        _ -> undefined
    end;
tag_hover(_, _) -> undefined.

tag_hover_tokens([{open_tag, _, _}, {_, {_, Row, Column}, Name} | Rest], Source, Position, Args)
    when is_binary(Name) ->
    Range = z_development_lsp_text:range(Source, Row, Column, Name),
    case at_position(Range, Position) of
        true ->
            case z_development_lsp_docs:tag(Name) of
                <<>> -> undefined;
                Text ->
                    Ref = #{
                        kind => tag,
                        name => z_development_lsp_docs:main_tag(Name),
                        documentation => Text
                    },
                    #{
                        contents => z_development_lsp_hover:format(
                                        Ref, [], [],
                                        maps:get(hover_format, Args, <<"plaintext">>)),
                        range => Range
                    }
            end;
        false -> tag_hover_tokens(Rest, Source, Position, Args)
    end;
tag_hover_tokens([_ | Rest], Source, Position, Args) ->
    tag_hover_tokens(Rest, Source, Position, Args);
tag_hover_tokens([], _, _, _) ->
    undefined.

%% @doc Compare actual targets, not the number of sites sharing a source directory.
%% Sites without a match do not make a uniquely resolvable reference ambiguous.
automatic_query(_, _, [], []) ->
    {error, <<"No running development site is available.">>};
automatic_query(completion, Args, Candidates, _) ->
    %% Suggestions do not select a rendering context, so their union is safe.
    lists:usort(lists:append([query(completion, Args, C) || C <- Candidates]));
automatic_query(Kind, Args, Candidates, All) when Kind =:= definition; Kind =:= hover ->
    Matches = [
        {C, Ref, Targets, reference_result(definition, Ref, Targets, <<>>, <<>>)}
        || C <- Candidates, {Ref, Targets} <- [resolve_selection(Args, C)], Targets =/= []
    ],
    case Matches of
        [] when Candidates =/= All -> automatic_query(Kind, Args, All, All);
        [] when Kind =:= definition -> [];
        [] -> null;
        [{_Context, Ref, Targets, Result} | _] ->
            %% Compare ordered canonical targets, preserving template priority.
            case lists:usort([R || {_, _, _, R} <- Matches]) of
                [_] when Kind =:= definition -> Result;
                [_] ->
                    Names = [atom_to_binary(z_context:site(C)) || {C, _, _, _} <- Matches],
                    reference_result(hover, Ref, Targets, Names, maps:get(hover_format, Args, <<"plaintext">>));
                _ ->
                    {error, <<"This reference resolves differently across sites; select a Zotonic site in the editor.">>}
            end
    end.

site_context(Site) ->
    z_context:new(binary_to_existing_atom(Site, utf8)).

%% @doc Classify the selected token without resolving it in any site.
server_reference(#{reference := #{kind := Kind}}) ->
    lists:member(Kind, [filter, model, delegate]);
server_reference(_) -> false.

%% @doc Check each distinct template root once, avoiding symlink walks for every file.
contains_file(File, Context) ->
    Path = filename:split(filter_development_filename:resolve(File)),
    Roots = lists:usort([
        filename:join(lists:sublist(filename:split(F), length(filename:split(F)) - length(filename:split(Name))))
        || #module_index{filepath = F, key = #module_index_key{name = Name}} <- z_module_indexer:all(template, Context)
    ]),
    lists:any(
        fun(Root) ->
            Parts = filename:split(filter_development_filename:resolve(unicode:characters_to_binary(Root))),
            lists:prefix(Parts, Path)
        end, Roots).

%% @doc Use the parser directly, keeping diagnostics available without a selected site.
-spec diagnostics(binary()) -> [map()].
diagnostics(Source) when byte_size(Source) =< ?MAX_SOURCE_BYTES ->
    case template_compiler_scanner:scan(<<"buffer">>, Source) of
        {ok, Tokens} ->
            case template_compiler_parser:parse(parser_tokens(Tokens)) of
                {ok, _} -> [];
                {error, {Location, Module, Reason}} ->
                    {Row, Column} = error_position(Location),
                    [diagnostic(Source, Row, Column, Module:format_error(Reason))]
            end;
        {error, Reason} -> [diagnostic(Source, 1, 1, Reason)]
    end.

%% @doc Match the compiler's tag-prefix disambiguation and trans token normalization.
%% No translation lookup, atom creation, code generation or evaluation is needed here.
parser_tokens(Tokens) ->
    Trimmed = lists:dropwhile(fun({Type, _, _}) -> Type =:= text end, Tokens),
    Normalized = case Trimmed of
        [{open_tag, _, _}, {Kind, _, _} | _]
            when Kind =:= extends_keyword;
                 Kind =:= overrules_keyword ->
                Trimmed;
        _ ->
            case Tokens of
                [{open_tag, Pos, _} | _] -> [{text, Pos, <<>>} | Tokens];
                _ -> Tokens
            end
    end,
    trans_tokens(Normalized).

trans_tokens([{trans_keyword, _, _} = Tag, {string_literal, Pos, Text} | Rest]) ->
    [Tag, {trans_literal, Pos, Text} | trans_tokens(Rest)];
trans_tokens([Token | Rest]) ->
    [Token | trans_tokens(Rest)];
trans_tokens([]) ->
    [].

error_position({_, Row, Column}) -> {max(1, Row), max(1, Column)};
error_position({Row, Column}) -> {max(1, Row), max(1, Column)};
error_position(Row) when is_integer(Row) -> {max(1, Row), 1};
error_position(_) -> {1, 1}.

diagnostic(Source, Row, Column, Message) ->
    #{
        range => z_development_lsp_text:range(Source, Row, Column, <<>>),
        severity => 1,
        source => <<"Zotonic">>,
        message => unicode:characters_to_binary(Message)
    }.

query(completion, #{source := Source, position := Position} = Args, Context) ->
    Prefix = z_development_lsp_text:prefix(Source, Position),
    case z_development_lsp_model_path:parse(Prefix) of
        undefined -> completion(Source, Prefix, Context);
        Match -> z_development_lsp_model:complete(Source, Prefix, Match,
            maps:get(site, Args, <<>>) =/= <<>>, Context)
    end;
query(Kind, Args, Context) when Kind =:= definition; Kind =:= hover ->
    case resolve_selection(Args, Context) of
        {Ref, Targets} ->
            reference_result(Kind, Ref, Targets,
                maps:get(site_label, Args, atom_to_binary(z_context:site(Context))),
                maps:get(hover_format, Args, <<"plaintext">>));
        undefined when Kind =:= definition -> [];
        undefined -> null
    end.

%% @doc Select a reference by position before resolving modules, templates or overrules.
reference_at_cursor(Source, Lines, Position) ->
    Refs = z_development_template:source_references(Source),
    Inheritance = case template_compiler_scanner:scan(<<"buffer">>, Source) of
        {ok, Tokens} ->
            [#{kind => template, name => <<"overrules">>, row => Row, column => Column,
               is_overrules => true} || {overrules_keyword, {_, Row, Column}, _} <- Tokens];
        _ -> []
    end,
    reference_at_position(Refs ++ Inheritance, Lines, Position).

reference_at_position([Ref | Rest], Lines, Position) ->
    Range = z_development_lsp_text:range(Lines, maps:get(row, Ref), maps:get(column, Ref), maps:get(name, Ref)),
    case at_position(Range, Position) of
        true -> Ref#{range => Range};
        false -> reference_at_position(Rest, Lines, Position)
    end;
reference_at_position([], _, _) -> undefined.

%% @doc Resolve only the cursor's reference; keep the result for hover formatting.
resolve_selection(#{reference := undefined}, _) -> undefined;
resolve_selection(#{reference := #{is_overrules := true} = Ref, file := File}, Context) ->
    Resolved = Ref#{matches => overrules(File, Context)},
    {Resolved, targets(Resolved, Context)};
resolve_selection(#{reference := Ref}, Context) ->
    case z_development_template:resolve_reference(Ref, Context) of
        undefined -> undefined;
        Resolved -> {Resolved, targets(Resolved, Context)}
    end.

overrules(File, Context) ->
    Path = filter_development_filename:resolve(File),
    case [T || #module_index{filepath = F} = T <- z_module_indexer:all(template, Context),
               filter_development_filename:resolve(unicode:characters_to_binary(F)) =:= Path] of
        [#module_index{key = #module_index_key{name = Name}, filepath = Indexed} | _] ->
            z_development_template:matching_templates({overrules, Name, Indexed}, Context);
        [] -> []
    end.

at_position(#{start := #{line := Row, character := Start}, 'end' := #{character := End}},
            #{<<"line">> := Row, <<"character">> := Character}) ->
    Character >= Start andalso Character =< End;
at_position(_, _) ->
    false.

reference_result(definition, _Ref, Targets, _Label, _Format) ->
    [
        #{
            uri => z_development_lsp_text:uri(File),
            range => line_range(Line)
        }
        || #{file := File, line := Line} <- Targets
    ];
reference_result(hover, Ref, Targets, Label, Format) ->
    #{
        contents => z_development_lsp_hover:format(Ref, Targets, Label, Format),
        range => maps:get(range, Ref)
    }.

targets(#{kind := template} = Ref, Context) ->
    Matches = case maps:find(matches, Ref) of
        {ok, Found} ->
            Found;
        error ->
            Name = maps:get(name, Ref),
            Option = case maps:get(is_category, Ref, false) of
                true -> {cat, Name};
                false -> Name
            end,
            z_development_template:matching_templates(Option, Context)
    end,
    [
        #{
            file => File,
            line => 1,
            owner => owner(Module)
        }
        || #{path := File, module := Module} <- Matches
    ];
targets(#{file := File, line := Line, erlang_module := Module}, _) ->
    Owner = case application:get_application(Module) of
        {ok, App} -> owner(App);
        undefined -> {<<"Erlang module">>, atom_to_binary(Module)}
    end,
    [
        #{
            file => File,
            line => Line,
            owner => Owner
        }
    ].

%% @doc Identify the source's owning application, independently of matching contexts.
owner(App) ->
    Name = case atom_to_binary(App) of
        <<"zotonic_mod_", Suffix/binary>> -> <<"mod_", Suffix/binary>>;
        Other -> Other
    end,
    Kind = case maps:is_key(App, z_sites_manager:get_sites()) of
        true -> <<"Site">>;
        false -> <<"Module">>
    end,
    {Kind, Name}.

line_range(Line) ->
    P = #{line => Line - 1, character => 0},
    #{start => P, 'end' => P}.

%% @doc Complete indexed template names, filters, models and tags in their syntactic context.
completion(Source, Prefix, Context) ->
    Types = [
        {template, <<"(?:include|catinclude|extends|compose|catcompose|template\\s*=)\\s*[\"']([^\"'\\r\\n]*)$">>},
        {filter, <<"\\|\\s*([a-zA-Z0-9_]*)$">>},
        {model, <<"\\bm\\.([a-zA-Z0-9_]*)$">>},
        {scomp, <<"\\{%\\s*([a-zA-Z0-9_]*)$">>}
    ],
    case [{Type, Part} || {Type, Pattern} <- Types,
            {match, [Part]} <- [re:run(Prefix, Pattern, [{capture, [1], binary}, unicode])]] of
        [{Type, Part} | _] ->
            Names = [
                z_convert:to_binary(Name)
                || #module_index{key = #module_index_key{name = Name}} <-
                    z_module_indexer:all_files(Type, zotonic_core) ++ z_module_indexer:all(Type, Context)
            ],
            All = case Type of
                scomp -> tags() ++ Names;
                _ -> Names
            end,
            EditRange = case Type of
                template -> z_development_lsp_text:completion_range(Source, Prefix, Part);
                _ -> undefined
            end,
            [
                maps:merge(completion_edit(EditRange, N), #{
                    label => N,
                    kind => completion_kind(Type),
                    detail => <<"Zotonic ", (atom_to_binary(Type))/binary>>
                })
                || N <- lists:usort(All), Part =:= <<>> orelse binary:match(N, Part) =:= {0, byte_size(Part)}];
        [] ->
            []
    end.

%% @doc Filename edits must replace directory prefixes and any existing suffix.
completion_edit(undefined, _) ->
    #{};
completion_edit(Range, Name) ->
    #{textEdit => #{range => Range, newText => Name}}.

completion_kind(template) -> 17;
completion_kind(_) -> 3.

tags() ->
    [<<"include">>, <<"catinclude">>, <<"extends">>, <<"overrules">>, <<"block">>,
     <<"endblock">>, <<"if">>, <<"else">>, <<"endif">>, <<"for">>, <<"endfor">>,
     <<"with">>, <<"endwith">>, <<"compose">>, <<"endcompose">>, <<"catcompose">>,
     <<"url">>, <<"wire">>, <<"javascript">>, <<"endjavascript">>, <<"lib">>].
