%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Format source locations, ownership and documentation for Markdown or plain-text editor hovers.
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

-module(z_development_lsp_hover).
-export([format/4]).

%% @doc Present canonical source locations in negotiated Markdown or plain text.
%% Values remain literal code/text: no HTML, command links or unescaped Markdown.
-spec format(map(), [map()], binary() | [binary()], binary()) -> map().
format(Ref, Targets, Site, Format) ->
    Kind = maps:get(kind, Ref),
    Category = maps:get(is_category, Ref, false),
    Paths = [
        {location(File, Line, Kind), Owner}
        || #{file := File, line := Line, owner := Owner} <- Targets
    ],
    Heading = case Format of
        <<"markdown">> ->
            [<<"**">>, title(Kind), <<"** ">>, code(maps:get(name, Ref))];
        _ ->
            [title(Kind), <<": ">>, maps:get(name, Ref)]
    end,
    Scope = scope(Site),
    Description = case {Kind, Category, Paths} of
        {tag, _, _} -> <<>>;
        {_, _, []} -> <<"No matching source found.">>;
        {template, true, _} -> <<"Category/resource candidates; the resource determines the effective template.">>;
        {template, false, _} -> <<"Module priority order; the first match is effective.">>;
        _ -> <<"Source implementation">>
    end,
    ScopeText = case Format of
        <<"markdown">> -> escape(Scope);
        _ -> Scope
    end,
    Lines = [
        path_line(Path, Index, Kind, Category, Format)
        || {Path, Index} <- lists:zip(Paths, lists:seq(1, length(Paths)))
    ],
    #{
        kind => Format,
        value => iolist_to_binary([
            Heading,
            case Scope of <<>> -> []; _ -> [<<"\n\n">>, ScopeText] end,
            <<"\n\n">>,
            Description,
            case Lines of [] -> []; _ -> [<<"\n\n">>, lists:join(<<"\n">>, Lines)] end,
            documentation(Ref)
        ])
    }.

%% @doc Append the repository documentation in the negotiated content format.
documentation(Ref) ->
    Doc = case Ref of
        #{kind := tag, documentation := Text} -> Text;
        #{erlang_module := Module} -> z_development_lsp_docs:module(Module);
        _ -> <<>>
    end,
    case Doc of
        <<>> -> [];
        _ -> [<<"\n\n">>, Doc]
    end.

%% @doc Resolve symlinks and display paths inside Zotonic relative to its root.
location(File, Line, Kind) ->
    Path = filter_development_filename:development_filename(filename:absname(File)),
    case Kind of
        template -> Path;
        _ -> <<Path/binary, ":", (integer_to_binary(Line))/binary>>
    end.

path_line({Path, {OwnerKind, Owner}}, Index, Kind, Category, Format) ->
    Label = case {Kind, Category, Index} of
        {template, false, 1} -> <<"Effective">>;
        {template, false, _} -> <<"Fallback">>;
        _ -> <<>>
    end,
    PathText = case {Format, Label} of
        {<<"markdown">>, <<>>} -> [<<"- ">>, code(Path)];
        {<<"markdown">>, _} -> [<<"- **">>, Label, <<"**: ">>, code(Path)];
        {_, <<>>} -> [<<"- ">>, Path];
        _ -> [<<"- ">>, Label, <<": ">>, Path]
    end,
    OwnerText = case Format of
        <<"markdown">> -> [OwnerKind, <<": ">>, code(Owner)];
        _ -> [OwnerKind, <<": ">>, Owner]
    end,
    [PathText, <<" — "/utf8>>, OwnerText].

%% @doc Keep small context lists useful without crowding shared-source hovers.
scope(Site) when is_binary(Site) -> <<"Site: ", Site/binary>>;
scope([]) -> <<>>;
scope([_, _, _, _ | _]) -> <<>>;
scope(Sites) -> iolist_to_binary([<<"Matching sites: ">>, lists:join(<<", ">>, Sites)]).

title(tag) -> <<"Tag">>;
title(template) -> <<"Template">>;
title(filter) -> <<"Filter">>;
title(model) -> <<"Model">>;
title(scomp) -> <<"Scomp">>;
title(action) -> <<"Action">>;
title(delegate) -> <<"Delegate">>.

%% @doc Use a delimiter longer than any backtick run in a source name or path.
code(Text) ->
    Flat = binary:replace(Text, [<<"\r">>, <<"\n">>], <<" ">>, [global]),
    Delimiter = binary:copy(<<"`">>, longest_ticks(Flat, 0, 0) + 1),
    [Delimiter, <<" ">>, Flat, <<" ">>, Delimiter].

longest_ticks(<<"`", Rest/binary>>, Run, Max) -> longest_ticks(Rest, Run + 1, max(Max, Run + 1));
longest_ticks(<<_, Rest/binary>>, _, Max) -> longest_ticks(Rest, 0, Max);
longest_ticks(<<>>, _, Max) -> Max.

%% @doc Escape Markdown punctuation in site labels without relying on HTML sanitizers.
escape(Text) ->
    [
        case       (C >= 33 andalso C =< 47)
            orelse (C >= 58 andalso C =< 64)
            orelse (C >= 91 andalso C =< 96)
            orelse (C >= 123 andalso C =< 126)
        of
            true -> [$\\, C];
            false -> C
        end || C <- binary_to_list(Text)
    ].
