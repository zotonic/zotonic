%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Translate file URIs and Unicode text positions between Zotonic scanners and the LSP protocol.
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

-module(z_development_lsp_text).
-export([uri/1, path/1, range/4, lines/1, completion_range/3, prefix/2, units/1]).

%% @doc Encode a local source path as a file URI, including spaces and Unicode.
-spec uri(file:filename_all()) -> binary().
uri(File) ->
    Path = filter_development_filename:resolve(unicode:characters_to_binary(filename:absname(File))),
    Encoded = uri_string:quote(Path, "/:"),
    <<"file://", Encoded/binary>>.

%% @doc Accept absolute local file URIs only; never fetch editor-supplied URLs.
-spec path(binary()) -> binary().
path(Uri) when is_binary(Uri) ->
    #{scheme := <<"file">>, path := Encoded} = Parsed = uri_string:parse(Uri),
    true = lists:member(maps:get(host, Parsed, <<>>), [<<>>, <<"localhost">>]),
    false = maps:is_key(query, Parsed) orelse maps:is_key(fragment, Parsed),
    Path = uri_string:percent_decode(Encoded),
    absolute = filename:pathtype(Path),
    nomatch = binary:match(Path, <<0>>),
    Path.

%% @doc Convert scanner codepoint positions (one-based) to LSP UTF-16 ranges.
-spec range(binary() | tuple(), pos_integer(), pos_integer(), binary()) -> map().
range(Source, Row, Column, Name) when is_binary(Source) ->
    range(lines(Source), Row, Column, Name);
range(Lines, Row, Column, Name) when is_tuple(Lines) ->
    Line = case tuple_size(Lines) >= Row of true -> element(Row, Lines); false -> <<>> end,
    Before = lists:sublist(unicode:characters_to_list(Line), Column - 1),
    Start = #{line => Row - 1, character => units(Before)},
    #{start => Start, 'end' => Start#{character => units(Before) + units(Name)}}.

%% @doc Index source lines once for constant-time lookup by scanner row.
-spec lines(binary()) -> tuple().
lines(Source) ->
    list_to_tuple(binary:split(Source, <<"\n">>, [global])).

%% @doc Replace a quoted filename, including its suffix beyond the cursor.
-spec completion_range(binary(), binary(), binary()) -> map().
completion_range(Source, Prefix, Part) ->
    Before = binary:part(Prefix, 0, byte_size(Prefix) - byte_size(Part)),
    BeforeLines = binary:split(Before, <<"\n">>, [global]),
    Start = #{line => length(BeforeLines) - 1, character => units(lists:last(BeforeLines))},
    Tail = binary:part(Source, byte_size(Prefix), byte_size(Source) - byte_size(Prefix)),
    {match, [Suffix]} = re:run(Tail, <<"^[^\"'\r\n]*">>, [{capture, first, binary}]),
    #{start => Start, 'end' => Start#{character => maps:get(character, Start) + units(Part) + units(Suffix)}}.

%% @doc Return text before a UTF-16 position, rejecting positions inside surrogate pairs.
-spec prefix(binary(), map()) -> binary().
prefix(Source, #{<<"line">> := Row, <<"character">> := Column})
    when is_integer(Row), Row >= 0, is_integer(Column), Column >= 0 ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    {Before, [Line | _]} = lists:split(Row, Lines),
    Chars = take_units(unicode:characters_to_list(Line), Column),
    iolist_to_binary(lists:join(<<"\n">>, Before ++ [unicode:characters_to_binary(Chars)])).

take_units(_, 0) -> [];
take_units([C | Rest], N) when C > 16#FFFF, N >= 2 -> [C | take_units(Rest, N - 2)];
take_units([C | Rest], N) when C =< 16#FFFF, N >= 1 -> [C | take_units(Rest, N - 1)].

%% @doc Count UTF-16 code units, independently of UTF-8 bytes or Unicode codepoints.
-spec units(unicode:chardata()) -> non_neg_integer().
units(Text) -> byte_size(unicode:characters_to_binary(Text, unicode, {utf16, little})) div 2.
