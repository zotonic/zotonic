%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'force_escape' filter, escape all html unsafe characters
%% @end

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

-module(filter_force_escape).
-export([force_escape/2]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


force_escape(undefined, _Context) ->
    <<>>;
force_escape({trans, _} = Trans, Context) ->
    force_escape(z_trans:lookup_fallback(Trans, Context), Context);
force_escape(true, _Context) ->
    filter_yesno:yesno(true, _Context);
force_escape(false, _Context) ->
    filter_yesno:yesno(false, _Context);
force_escape(Input, _Context) when is_atom(Input) ->
    escape1(atom_to_binary(Input, utf8), []);
force_escape(Input, _Context) when is_list(Input) ->
    escape1(Input, []);
force_escape(Input, _Context) when is_binary(Input) ->
    escape1(Input, 0);
force_escape(Input, _Context) when is_integer(Input) ->
    integer_to_binary(Input);
force_escape(Input, _Context) when is_float(Input) ->
    z_convert:to_binary(Input);
force_escape({{Y,M,D}, {_H,_I,_S}} = Input, Context) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    filter_date:date(Input, "Y-m-d H:i:s", Context);
force_escape({Y,M,D} = Input, Context) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    filter_date:date(Input, "Y-m-d", Context);
force_escape(Input, Context) when is_map(Input) ->
    JSON = z_json:encode(Input),
    force_escape(JSON, Context).


escape1(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, $<, Post/binary>> ->
            process_binary_match(Pre, <<"&lt;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, $>, Post/binary>> ->
            process_binary_match(Pre, <<"&gt;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"&quot;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"&#039;">>, size(Post), escape1(Post, 0));
        <<_:Index/binary, _, _/binary>> ->
            escape1(Binary, Index + 1);
        Binary ->
            Binary
    end;
escape1([], Acc) ->
    lists:reverse(Acc);
escape1("<" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&lt;", Acc));
escape1(">" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&gt;", Acc));
escape1("&" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&amp;", Acc));
escape1("\"" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&quot;", Acc));
escape1("'" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&#039;", Acc));
escape1([C | Rest], Acc) ->
    escape1(Rest, [C | Acc]).


process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
