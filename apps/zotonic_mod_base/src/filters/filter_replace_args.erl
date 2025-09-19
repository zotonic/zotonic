%% @author Andreas Stenius <andreas.stenius@astekk.se>
%% @copyright 2011 Andreas Stenius
%% @doc 'replace_args' filter, replace $N in string from a list of replacement values.

%% Copyright 2011 Andreas Stenius
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

%% Replace any $N in Input with the N'th value from Args.
%%
%% Example usage: {{ value|replace_args:["first", "second", "third"] }}
%%   For a value of "My $1 trough $3 is not $2 to best"
%%   produces "My first trough third is not second to best"
%%
%% $ may be escaped by \\ to avoid replacement.
%% N may be in the range [1..9]. If N is out of range of the provided
%% args, the $N is left as-is.
%%
%% For single arg, {{ value|replace_args:"first" }} is also allowed.

-module(filter_replace_args).
-moduledoc("
Replace `$N` placeholders in string from a list of replacement values.

The input string is searched for any `$N` tags, which is replaced with the N‘th value from the list of arguments
supplied to the filter.

Example usage:


```django
{{ \"Replace $1 and give it some $2.\"|replace_args:[\"item\", \"meaning\"] }}
```

Will result in: “Replace item and give it some meaning.”

The `$N` tag may be escaped by \\ to avoid replacement.

N may be in the range \\[1..9\\]. If N is out of range of the provided args, the $N tag is left as-is.

The $N tags may come in any order, any number of times.

When replacing a single tag, the replacement argument may be given directly:


```django
{{ \"John is $1\"|replace_args:\"the one\" }}
```

Outputs: “John is the one”

New in version 0.8.
").
-export([
         replace_args/2,
         replace_args/3
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

replace_args(Input, _Context) ->
    Input.

replace_args(Input, Args, _Context) when is_list(Input) andalso is_list(Args) ->
    case Args of
        [H|_] when is_list(H) ->
            do_replace_args(Input, Args, []);
        Arg ->
            do_replace_args(Input, [Arg], [])
    end;
replace_args(Input, Args, Context) when is_binary(Input) ->
    replace_args(unicode:characters_to_list(Input), Args, Context);
replace_args(Input, _Args, _Context) ->
    Input.

do_replace_args([$\\, $$|Input], Args, Acc) ->
    do_replace_args(Input, Args, [$$|Acc]);
do_replace_args([$$, N|Input], Args, Acc) ->
    case N - $0 of
        Nth when Nth >= 1 andalso Nth =< length(Args) ->
            do_replace_args(Input, Args, [lists:nth(Nth, Args)|Acc]);
        _ ->
            do_replace_args(Input, Args, [N, $$|Acc])
    end;
do_replace_args([H|Input], Args, Acc) ->
    do_replace_args(Input, Args, [H|Acc]);
do_replace_args([], _Args, Acc) ->
    lists:reverse(Acc).
