%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Convert Markdown tables to labelled lists for editor hovers that cannot render tables.
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

-module(z_development_lsp_tables).
-export([to_lists/1]).

%% @doc Convert Markdown tables to labelled lists, preserving fenced code examples.
-spec to_lists(binary()) -> binary().
to_lists(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    iolist_to_binary(lists:join(<<"\n">>, lines(Lines, none))).

lines([], _) -> [];
lines([Line | Rest], {Char, Size} = Fence) ->
    Next = case fence(Line) of
        {Char, N, Tail} when N >= Size ->
            case string:trim(Tail) of <<>> -> none; _ -> Fence end;
        _ -> Fence
    end,
    [Line | lines(Rest, Next)];
lines([Line | Rest], none) ->
    case fence(Line) of
        {Char, Size, _} -> [Line | lines(Rest, {Char, Size})];
        none -> table_or_line(Line, Rest)
    end.

fence(Line) ->
    case re:run(Line, <<"^ {0,3}(`{3,}|~{3,})(.*)$">>, [{capture, [1, 2], binary}]) of
        {match, [Run, Tail]} -> {binary:first(Run), byte_size(Run), Tail};
        nomatch -> none
    end.

table_or_line(Header, [Separator | Rest]) ->
    Headers = cells(Header),
    Separators = cells(Separator),
    IsTable = length(Headers) >= 2 andalso length(Headers) =:= length(Separators)
        andalso lists:all(fun separator/1, Separators),
    case IsTable of
        true ->
            {Rows, Remaining} = rows(Rest, length(Headers), []),
            case Rows of
                [] -> [Header | lines([Separator | Rest], none)];
                _ -> [<<>>] ++ [row(Headers, R) || R <- Rows] ++ [<<>> | lines(Remaining, none)]
            end;
        false -> [Header | lines([Separator | Rest], none)]
    end;
table_or_line(Line, []) -> [Line].

separator(Cell) ->
    re:run(Cell, <<"^:?-{3,}:?$">>, [{capture, none}]) =:= match.

rows([Line | Rest] = Remaining, Count, Acc) ->
    Cells = cells(Line),
    case length(Cells) =:= Count andalso fence(Line) =:= none of
        true -> rows(Rest, Count, [Cells | Acc]);
        false -> {lists:reverse(Acc), Remaining}
    end;
rows([], _, Acc) -> {lists:reverse(Acc), []}.

row(Headers, Cells) ->
    Fields = [[H, <<": ">>, C] || {H, C} <- lists:zip(Headers, Cells)],
    [<<"- ">>, lists:join(<<"; ">>, Fields)].

%% @doc Split unescaped pipes while keeping Markdown escapes intact and ignoring code indentation.
cells(<<"    ", _/binary>>) -> [];
cells(<<"\t", _/binary>>) -> [];
cells(Line) ->
    Parts = split(string:trim(Line), [], [], 0),
    Trimmed = [string:trim(P) || P <- Parts],
    drop_edge(lists:reverse(drop_edge(lists:reverse(Trimmed)))).

drop_edge([<<>> | Rest]) -> Rest;
drop_edge(Parts) -> Parts.

%% @doc Pipes inside code spans belong to examples, not to table columns.
split(<<$\\, C, Rest/binary>>, Cell, Acc, Ticks) -> split(Rest, [C, $\\ | Cell], Acc, Ticks);
split(<<$`, _/binary>> = Text, Cell, Acc, Ticks) ->
    {Count, Rest} = ticks(Text, 0),
    Next = case Ticks of
        0 ->
            case has_closing_ticks(Rest, Count) of
                true -> Count;
                false -> 0
            end;
        Count -> 0;
        _ -> Ticks
    end,
    split(Rest, lists:duplicate(Count, $`) ++ Cell, Acc, Next);
split(<<$|, Rest/binary>>, Cell, Acc, 0) ->
    split(Rest, [], [list_to_binary(lists:reverse(Cell)) | Acc], 0);
split(<<C, Rest/binary>>, Cell, Acc, Ticks) -> split(Rest, [C | Cell], Acc, Ticks);
split(<<>>, Cell, Acc, _) -> lists:reverse([list_to_binary(lists:reverse(Cell)) | Acc]).

ticks(<<$`, Rest/binary>>, Count) -> ticks(Rest, Count + 1);
ticks(Rest, Count) -> {Count, Rest}.

%% @doc An unmatched backtick run is literal Markdown, not an open code span.
has_closing_ticks(<<$`, _/binary>> = Text, Count) ->
    case ticks(Text, 0) of
        {Count, _} -> true;
        {_, Rest} -> has_closing_ticks(Rest, Count)
    end;
has_closing_ticks(<<_, Rest/binary>>, Count) -> has_closing_ticks(Rest, Count);
has_closing_ticks(<<>>, _) -> false.
