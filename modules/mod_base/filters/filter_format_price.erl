%% @author Michael Connors <michael@bring42.net>
%% @copyright 2010 Michael Connors
%% @doc 'format_price' filter, show a price with two digits. Accepts a price in cents.

%% Copyright 2010 Michael Connors
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

-module(filter_format_price).
-export([format_price/4, format_price/3, format_price/2]).

insert_thousands_separator(_Sep, Output, []) ->
    Output;

insert_thousands_separator(Sep, Output, Input) when is_list(Input) ->
    case length(Input) > 3 of
        true ->
            case length(Input) rem 3 of
                0 -> 
                     [Head | Input2] = Input,
                     insert_thousands_separator(Sep, lists:append(Output, [Head]), Input2);
                1 -> 
                     [Head | Input2] = Input,
                     Head1 = lists:append([Head], [Sep]),
                     insert_thousands_separator(Sep, lists:append(Output, Head1), Input2);
                2 ->
                     [Head | Input2] = Input,
                     insert_thousands_separator(Sep, lists:append(Output, [Head]), Input2)
            end;
        false -> lists:append(Output, Input)
    end.

insert_thousands_separator(Sep, Input) when is_integer(Input) ->
    insert_thousands_separator(Sep, [], integer_to_list(Input)).

format_price(Input, DSep, TSep, _Context) when is_integer(Input) ->
    case Input rem 100 of
        0 -> 
            [insert_thousands_separator(TSep, Input div 100), DSep, $0, $0 ];
        Cents when Cents < 10 -> 
            [insert_thousands_separator(TSep, Input div 100), DSep, $0, Cents + $0 ];
        Cents -> 
            [insert_thousands_separator(TSep, Input div 100), DSep, integer_to_list(Cents) ]
    end;
format_price(Input, DSep, TSep, Context) when is_float(Input) ->
    format_price(round(Input * 100), DSep, TSep, Context);
format_price(Input, DSep, TSep, Context) when is_function(Input, 0) ->
    format_price(Input(), DSep, TSep, Context);
format_price(Input, DSep, TSep, Context) when is_function(Input, 1) ->
    format_price(Input(Context), DSep, TSep, Context);
format_price(Input, DSep, TSep, Context) when is_list(Input) ->
    case string:to_integer(Input) of
        {error, _} -> Input;
        {N, _Rest} -> format_price(N, DSep, TSep, Context)
    end;
format_price(Input, DSep, TSep, Context) when is_binary(Input) ->
    case string:to_integer(binary_to_list(Input)) of
        {error, _} -> Input;
        {N, _Rest} -> format_price(N, DSep, TSep, Context)
    end;
format_price(undefined, _Dsep, _Tsep, _Context) ->
    "-".

format_price(Input, Args, Context) ->
    case length(Args) of
        0 -> format_price(Input, $., $,, Context);
        1 -> format_price(Input, Args, $,, Context);
        2 -> [DSep, TSep] = Args,
             format_price(Input, DSep, TSep, Context);
        _ -> format_price(Input, $., $,, Context)
    end.

format_price(Input, Context) ->
    format_price(Input, $., $,, Context).
