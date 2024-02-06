%% @author Marc Worrell
%% @copyright 2020 Marc Worrell
%% @doc 'format_duration' filter, show a duration in seconds as readable text.

%% Copyright 2020 Marc Worrell
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

-module(filter_format_duration).
-export([format_duration/2]).


format_duration(Input, Context) when is_number(Input), Input < 0 ->
    [ <<"-">>, format_duration( 0 - Input, Context ) ];
format_duration(Input, _Context) when is_number(Input), Input >= 0 ->
    Input1 = erlang:round(Input),
    Hrs = Input1 div 3600,
    Mins = (Input1 div 60) rem 60,
    Secs = Input1 rem 60,
    case {Hrs, Mins, Secs} of
        {0, 0, _} ->
            [ integer_to_binary(Secs), $s ];
        {0, _, 0} ->
            [ integer_to_binary(Mins), $m ];
        {0, _, _} ->
            [ integer_to_binary(Mins), $m, integer_to_binary(Secs), $s ];
        {_, 0, 0} ->
            [ integer_to_binary(Hrs), $h ];
        {_, _, 0} ->
            [ integer_to_binary(Hrs), $h, integer_to_binary(Mins), $m ];
        _ ->
            [ integer_to_binary(Hrs), $h, integer_to_binary(Mins), $m, integer_to_binary(Secs), $s ]
    end;
format_duration(Input, _Context) ->
    Input.

