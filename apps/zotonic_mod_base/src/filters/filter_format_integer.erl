%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @author    Evan Miller <emmiller@gmail.com>
%% @copyright 2008 Roberto Saccon, Evan Miller
%% @doc 'format_integer' filter, return string representation of an integer

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

-module(filter_format_integer).
-moduledoc("
See also

[to\\_integer](/id/doc_template_filter_filter_to_integer), [format\\_number](/id/doc_template_filter_filter_format_number), [format\\_price](/id/doc_template_filter_filter_format_price), [format\\_duration](/id/doc_template_filter_filter_format_duration)

Show an integer value.

Formats an integer value as a list, assuming a radix of ten.

For example:


```django
{{ value|format_integer }}
```

When the value is the integer 123 then the output is the list `123`.

The format\\_integer filter has an optional argument to pad the returned string with zeros to a fixed length. For example:


```django
{{ 123|format_integer:5 }}
```

Will output `00123`. And when the number does not fit:


```django
{{ 123|format_integer:2 }}
```

Will output “\\*\\*”.

**Note:** This option only works for positive integers.
").
-export([format_integer/2, format_integer/3]).

-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


format_integer(Input, _Context) when is_integer(Input) ->
    integer_to_list(Input);
format_integer(Input, _Context) ->
    Input.


format_integer(Input, Len, _Context) when is_integer(Input) ->
    S = integer_to_list(Input),
	case Len - length(S) of
		N when N > 0 -> lists:duplicate(N, $0) ++ S;
		N when N < 0 -> lists:duplicate(Len, $*);
		0 -> S
	end;
format_integer(Input, _Len, _Context) ->
    Input.
