%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Read incomplete model expressions without evaluating template arguments.
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

-module(z_development_lsp_model_path).
-export([parse/1, edit/3]).

%% @doc Find a model path at the cursor, skipping quoted text and balanced arguments.
-spec parse(binary()) -> undefined | map().
parse(Prefix) ->
    %% HTML quotes do not delimit template expressions; template quotes do.
    case expression(unicode:characters_to_list(Prefix), outside, []) of
        undefined -> undefined;
        Text -> scan(Text, false)
    end.

expression([], outside, _) -> undefined;
expression([], comment, _) -> undefined;
expression([], _, Acc) -> lists:reverse(Acc);
expression([${, ${ | Rest], outside, _) -> expression(Rest, body, []);
expression([${, $% | Rest], outside, _) -> expression(Rest, body, []);
expression([${, $# | Rest], outside, _) -> expression(Rest, comment, []);
expression([$#, $} | Rest], comment, _) -> expression(Rest, outside, []);
expression([_, _ | _] = Text, body, Acc) -> expression_body(Text, Acc);
expression([$\\, C | Rest], {quote, Q}, Acc) -> expression(Rest, {quote, Q}, [C, $\\ | Acc]);
expression([Q | Rest], {quote, Q}, Acc) -> expression(Rest, body, [Q | Acc]);
expression([C | Rest], body, Acc) when C =:= $"; C =:= $' -> expression(Rest, {quote, C}, [C | Acc]);
expression([_ | Rest], outside, _) -> expression(Rest, outside, []);
expression([_ | Rest], comment, _) -> expression(Rest, comment, []);
expression([C | Rest], State, Acc) -> expression(Rest, State, [C | Acc]).

expression_body([$}, $} | Rest], _) -> expression(Rest, outside, []);
expression_body([$%, $} | Rest], _) -> expression(Rest, outside, []);
expression_body([Q | Rest], Acc) when Q =:= $"; Q =:= $' -> expression(Rest, {quote, Q}, [Q | Acc]);
expression_body([C | Rest], Acc) -> expression(Rest, body, [C | Acc]).

scan([], _) ->
    undefined;
scan([$m, $. | Rest], false) ->
    case identifier(Rest, []) of
        {[], _} -> scan(Rest, false);
        {Model, Tail} ->
            case path(Tail, [], 0) of
                undefined -> scan(Rest, false);
                Match -> Match#{model => unicode:characters_to_binary(Model)}
            end
    end;
scan([Q | Rest], _) when Q =:= $"; Q =:= $' ->
    case quoted(Rest, Q, []) of
        {closed, _, Tail} -> scan(Tail, false);
        _ -> undefined
    end;
scan([C | Rest], _) ->
    scan(Rest, is_identifier(C) orelse C =:= $.).

path(_, _, N) when N > 64 ->
    undefined;
path([$. | Rest], Parts, N) ->
    case identifier(Rest, []) of
        {Part, []} -> result(Parts, Part, dot);
        {[], _} -> undefined;
        {Part, Tail} -> path(Tail, [unicode:characters_to_binary(Part) | Parts], N + 1)
    end;
path([$[, Q | Rest], Parts, N) when Q =:= $"; Q =:= $' ->
    case quoted(Rest, Q, []) of
        {open, Part} -> result(Parts, Part, Q);
        {closed, Part, [$] | Tail]} -> path(Tail, [unicode:characters_to_binary(Part) | Parts], N + 1);
        _ -> undefined
    end;
path([$[ | Rest], Parts, N) ->
    case bracket(Rest, 1) of
        {ok, Tail} -> path(Tail, [any | Parts], N + 1);
        _ -> undefined
    end;
path(_, _, _) ->
    undefined.

result(Parts, Part, Quote) ->
    #{
        path => lists:reverse(Parts),
        part => unicode:characters_to_binary(Part),
        quote => Quote
    }.

identifier([C | Rest] = All, Acc) ->
    case is_identifier(C) of
        true -> identifier(Rest, [C | Acc]);
        false -> {lists:reverse(Acc), All}
    end;
identifier([], Acc) ->
    {lists:reverse(Acc), []}.

is_identifier(C) ->
    (C >= $a andalso C =< $z)
    orelse (C >= $A andalso C =< $Z)
    orelse (C >= $0 andalso C =< $9)
    orelse C =:= $_.

%% Escaped strings are intentionally not inferred: their source and value lengths differ.
quoted([$\\ | _], _, _) -> invalid;
quoted([Q | Rest], Q, Acc) -> {closed, lists:reverse(Acc), Rest};
quoted([C | Rest], Q, Acc) when C =/= $\n, C =/= $\r -> quoted(Rest, Q, [C | Acc]);
quoted([], _, Acc) -> {open, lists:reverse(Acc)};
quoted(_, _, _) -> invalid.

bracket(_, N) when N > 64 -> invalid;
bracket([$] | Rest], 1) -> {ok, Rest};
bracket([$] | Rest], N) -> bracket(Rest, N - 1);
bracket([$[ | Rest], N) -> bracket(Rest, N + 1);
bracket([Q | Rest], N) when Q =:= $"; Q =:= $' ->
    case quoted(Rest, Q, []) of
        {closed, _, Tail} -> bracket(Tail, N);
        _ -> invalid
    end;
bracket([_ | Rest], N) -> bracket(Rest, N);
bracket([], _) -> invalid.

%% @doc Replace only the current segment, including a suffix beyond the cursor.
-spec edit(binary(), binary(), map()) -> map().
edit(Source, Prefix, #{part := Part, quote := Quote}) ->
    Before = binary:part(Prefix, 0, byte_size(Prefix) - byte_size(Part)),
    Lines = binary:split(Before, <<"\n">>, [global]),
    Start = #{line => length(Lines) - 1, character => z_development_lsp_text:units(lists:last(Lines))},
    Tail = binary:part(Source, byte_size(Prefix), byte_size(Source) - byte_size(Prefix)),
    Suffix = suffix(unicode:characters_to_list(Tail), Quote),
    #{
        start => Start,
        'end' => Start#{
            character => maps:get(character, Start)
                    + z_development_lsp_text:units(Part)
                    + z_development_lsp_text:units(Suffix)
        }
    }.

suffix(Tail, dot) -> element(1, identifier(Tail, []));
suffix(Tail, Q) -> lists:takewhile(fun(C) -> C =/= Q andalso C =/= $\n andalso C =/= $\r end, Tail).
