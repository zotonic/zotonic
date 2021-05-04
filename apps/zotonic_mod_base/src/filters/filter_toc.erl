%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Parse HTML, adds ids to all header elements and returns a toc menu referring to those ids.

%% Copyright 2010 Marc Worrell
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
-module(filter_toc).

-export([
    toc/2,

    test/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").

toc(undefined, _Context) ->
    {[], <<>>};
toc(#trans{} = Tr, Context) ->
    toc(z_trans:lookup_fallback(Tr, Context), Context);
toc(B, _Context) when is_binary(B) ->
    {Toc, Html1} = parse_toc(B, $1, [], [], <<>>),
    {nested(lists:reverse(Toc)), <<"<div>", Html1/binary, "</div>">>};
toc(V, Context) ->
    toc(z_convert:to_binary(V), Context).

parse_toc(<<>>, _Level, _Path, Toc, Html) ->
    {Toc, Html};
parse_toc(<<"<h", N, Rest/binary>>, Lvl, Path, Toc, Html) when N >= $2, N =< $6 ->
    if
        N =< Lvl ->
            % Pop Lvl - N items from prefix
            % Start count from 1 again.
            [ Count | Path1 ] = pop(Lvl - N, Path),
            Count1 = Count+1,
            Path2 = [ Count1 | Path1 ],
            Html1 = <<Html/binary, "</div><div id=\"", (prefix_id(Path2))/binary, "\"><h", N>>,
            Toc1 = [ {Path2, header_text(Rest)} | Toc ],
            parse_toc(Rest, N, Path2, Toc1, Html1);
        N > Lvl ->
            % Push items to prefix
            Path1 = push(N - Lvl, Path),
            Html1 = <<Html/binary, "</div><div id=\"", (prefix_id(Path1))/binary, "\"><h", N>>,
            Toc1 = [ {Path1, header_text(Rest)} | Toc ],
            parse_toc(Rest, N, Path1, Toc1, Html1)
    end;
parse_toc(<<C/utf8, Rest/binary>>, Lvl, Prefix, Toc, Html) ->
    parse_toc(Rest, Lvl, Prefix, Toc, <<Html/binary, C/utf8>>).


pop(_, []) -> [];
pop(0, L) -> L;
pop(N, L) -> pop(N-1, tl(L)).

push(0, L) -> L;
push(N, L) -> push(N-1, [ 1 | L ]).

prefix_id(L) ->
    L1 = lists:map( fun integer_to_binary/1, L),
    iolist_to_binary(["toc", lists:join($-, lists:reverse(L1)) ]).


nested(Toc) ->
    {Nested, _} = nested(Toc, 1, []),
    Nested.

nested([], _Depth, Acc) ->
    {lists:reverse(Acc), []};
nested([ {T, Text} | Ts ] = TTs, Depth, Acc) ->
    TLen = length(T),
    if
        TLen =:= Depth ->
            {Sub, Ts1} = nested(Ts, Depth+1, []),
            nested(Ts1, Depth, [ {prefix_id(T), Text, Sub} | Acc ]);
        TLen > Depth ->
            {Sub, Ts1} = nested(TTs, Depth+1, []),
            nested(Ts1, Depth, [ {undefined, <<>>, Sub} | Acc ]);
        TLen < Depth ->
            {lists:reverse(Acc), TTs}
    end.

header_text(B) ->
    z_string:trim(z_html:strip(header_text(B, <<>>))).

header_text(<<>>, Acc) ->
    Acc;
header_text(<<"</h", _/binary>>, Acc) ->
    Acc;
header_text(<<C/utf8, Rest/binary>>, Acc) ->
    header_text(Rest, <<Acc/binary, C/utf8>>).


test() ->
    Html = <<"
        <h2>1.</h2
        <h3>1.1</h3>
        <h4>1.1.1</h4>
        <h3>1.2</h3>
        <h4>1.2.1</h4>
        <h4>1.2.2</h4>
        <h2>2.</h2>
        <h5>2.1.1.1</h5>
    ">>,
    toc(Html, x).

