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
-export([
         replace_args/2,
         replace_args/3
        ]).

-include("zotonic.hrl").

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
    replace_args(z_convert:to_list(Input), Args, Context);
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
