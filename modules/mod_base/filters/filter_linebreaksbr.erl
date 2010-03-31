%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'linebreaksbr' filter, translate linebreaks into <br/> elements

%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(filter_linebreaksbr).
-export([linebreaksbr/2]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


linebreaksbr(undefined, _Context) ->
    undefined;
linebreaksbr(Input, _Context) when is_binary(Input) ->
    linebreaksbr1(Input, 0);
linebreaksbr(Input, _Context) ->
    linebreaksbr1(Input, []).

linebreaksbr1(Input, Index) when is_binary(Input) ->
    Break = <<"<br />">>,
    case Input of
        <<Pre:Index/binary, $\r, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr1(Post, 0));
        <<Pre:Index/binary, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr1(Post, 0));
        <<_:Index/binary, _/binary>> ->
            linebreaksbr1(Input, Index + 1);
        _ ->
            Input
    end;
linebreaksbr1([], Acc) ->
    lists:reverse(Acc);
linebreaksbr1("\r\n" ++ Rest, Acc) ->
    linebreaksbr1(Rest, lists:reverse("<br />", Acc));
linebreaksbr1("\n" ++ Rest, Acc) ->
    linebreaksbr1(Rest, lists:reverse("<br />", Acc));
linebreaksbr1([C | Rest], Acc) ->
    linebreaksbr1(Rest, [C | Acc]).


process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
