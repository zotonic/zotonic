%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'format_number' filter, return string representation of an integer or float

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

-module(filter_format_number).
-export([format_number/2]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


format_number(Input, _Context) when is_integer(Input) ->
    integer_to_list(Input);
format_number(Input, _Context) when is_float(Input) ->
    io_lib:format("~p", [Input]);
format_number(Input, Context) when is_function(Input, 0) ->
    format_number(Input(), Context);
format_number(Input, Context) when is_function(Input, 1) ->
    format_number(Input(Context), Context);
format_number(Input, _Context) ->
    Input.
