%% @copyright 2026 Marc Worrell
%% @doc Remove configured patterns from decoded text, preserving surrounding HTML.
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

-module(z_import_feed_cleanup).
-define(MAX_PATTERN_BYTES, 16384).
-define(MAX_PATTERNS, 50).
-define(MATCH_LIMIT, 100000).
-define(MATCH_RECURSION_LIMIT, 10000).
-export([compile/1, text/2, html/2]).

%% One Unicode regexp per line. Limits bound work on externally supplied content.
-spec compile(binary()) -> {ok, list()} | {error, term()}.
compile(Input) when is_binary(Input), byte_size(Input) =< ?MAX_PATTERN_BYTES ->
    Lines = [z_string:trim(LineText) || LineText <- binary:split(Input, <<"\n">>, [global])],
    case length(Lines) =< ?MAX_PATTERNS of
        true -> compile_lines(Lines, 1, []);
        false -> {error, too_many_patterns}
    end;
compile(_) -> {error, patterns_too_long}.

compile_lines([], _, Acc) -> {ok, lists:reverse(Acc)};
compile_lines([<<>> | Rest], Line, Acc) -> compile_lines(Rest, Line + 1, Acc);
compile_lines([Pattern | Rest], Line, Acc) ->
    case re:compile(Pattern, [unicode, ucp, multiline]) of
        {ok, CompiledPattern} -> compile_lines(Rest, Line + 1, [CompiledPattern | Acc]);
        {error, {Reason, _}} -> {error, {invalid_regexp, Line, Reason}}
    end.

-spec text(binary(), list()) -> binary().
text(Text, Patterns) -> lists:foldl(fun(CompiledPattern, Acc) -> remove(Acc, ranges(Acc, CompiledPattern), 0) end, Text, Patterns).

-spec html(binary(), list()) -> binary().
html(Html, []) -> Html;
html(Html, Patterns) ->
    case z_html_parse:parse(<<"<div>", Html/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            Clean = lists:foldl(fun(CompiledPattern, Nodes) ->
                Plain = iolist_to_binary(plain(Nodes)),
                {Result, _} = clean(Nodes, ranges(Plain, CompiledPattern), 0),
                Result
            end, Children, Patterns),
            iolist_to_binary([case ChildNode of
                TextNode when is_binary(TextNode) -> z_html_parse:escape(TextNode);
                _ -> z_html_parse:to_html(ChildNode)
            end || ChildNode <- Clean]);
        _ -> Html
    end.

plain(Nodes) -> [case ChildNode of
    TextNode when is_binary(TextNode) -> TextNode;
    {_, _, Children} -> plain(Children);
    _ -> <<>>
end || ChildNode <- Nodes].

ranges(Text, CompiledPattern) ->
    case re:run(Text, CompiledPattern, [global, {capture, first, index},
            {match_limit, ?MATCH_LIMIT}, {match_limit_recursion, ?MATCH_RECURSION_LIMIT}, report_errors]) of
        {match, Matches} -> [{Start, Start + Length} || [{Start, Length}] <- Matches, Length > 0];
        nomatch -> [];
        {error, Reason} -> throw({import_error, {removal_regexp, Reason}})
    end.

clean([], _, Offset) -> {[], Offset};
clean([Node | Rest], Ranges, Offset) ->
    {Nodes, Next} = case Node of
        TextNode when is_binary(TextNode) -> {[remove(TextNode, Ranges, Offset)], Offset + byte_size(TextNode)};
        {Tag, Attributes, Children} ->
            {Clean, End} = clean(Children, Ranges, Offset),
            Keep = [ChildNode || ChildNode <- Clean, ChildNode =/= <<>>],
            case Keep =:= [] andalso Children =/= [] of
                true -> {[], End};
                false -> {[{Tag, Attributes, Keep}], End}
            end;
        _ -> {[Node], Offset}
    end,
    {Tail, Final} = clean(Rest, Ranges, Next),
    {Nodes ++ Tail, Final}.

remove(Text, [], _) -> Text;
remove(Text, [{Start, End} | Rest], Offset) ->
    Size = byte_size(Text),
    case End =< Offset orelse Start >= Offset + Size of
        true -> remove(Text, Rest, Offset);
        false ->
            From = max(0, Start - Offset),
            To = min(Size, End - Offset),
            Prefix = binary:part(Text, 0, From),
            Tail = binary:part(Text, To, Size - To),
            <<Prefix/binary, (remove(Tail, Rest, Offset + To))/binary>>
    end.
