%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'ljust' filter, left justify in a fixed length field

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

-module(filter_ljust).
-export([ljust/3]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


ljust(undefined, _Number, _Context) -> 
    undefined;
ljust(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(ljust(binary_to_list(Input), Number, Context));
ljust(Input, Number, _Context) when is_list(Input) ->
    string:left(Input, z_convert:to_integer(Number));
ljust(Input, _Number, _Context) -> 
    Input.
