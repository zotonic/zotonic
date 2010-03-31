%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'urlencode' filter, translate a string to url safe characters

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

-module(filter_urlencode).
-export([urlencode/2]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-define(NO_ENCODE(C), ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $. orelse C =:= $- 
                                  orelse C =:= $~ orelse C =:= $_))).


urlencode(undefined, _Context) ->
    undefined;
urlencode(Input, _Context) when is_binary(Input) ->
    urlencode1(Input, 0);
urlencode(Input, _Context) when is_list(Input) ->
    urlencode1(Input, []);
urlencode(_Input, _Context) ->
    <<>>.


urlencode1(Input, Index) when is_binary(Input) ->
    case Input of
        <<_:Index/binary, Byte, _/binary>> when ?NO_ENCODE(Byte) ->
            urlencode1(Input, Index + 1);
        <<Pre:Index/binary, $\s, Post/binary>> ->
            process_binary_match(Pre, <<"+">>, size(Post), urlencode1(Post, 0));
        <<Pre:Index/binary, Hi:4, Lo:4, Post/binary>> ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), urlencode1(Post, 0));
        Input ->
            Input
    end;
urlencode1([], Acc) ->
    lists:reverse(Acc);
urlencode1([C | Rest], Acc) when ?NO_ENCODE(C) ->
    urlencode1(Rest, [C | Acc]);
urlencode1([$\s | Rest], Acc) ->
    urlencode1(Rest, [$+ | Acc]);
urlencode1([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode1(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.


