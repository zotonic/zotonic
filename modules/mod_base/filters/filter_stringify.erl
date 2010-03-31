%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'stringify' filter, translate atoms and numbers to strings

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

-module(filter_stringify).
-export([stringify/2]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


% Translate atoms and numbers to strings
% Leave tuples as tuples.
stringify(undefined, _Context) ->
    <<>>;
stringify(In, _Context) when is_atom(In) ->
    atom_to_list(In);
stringify(In, _Context) when is_integer(In) ->
    integer_to_list(In);
stringify(In, _Context) when is_float(In) ->
    mochinum:digits(In);
stringify(In, _Context) ->
    In.
