%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'format_price' filter, show a price with two digits. Accepts a price in cents.

%% Copyright 2010 Marc Worrell
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
-export([format_price/2]).


format_price(Input, _Context) when is_integer(Input) ->
    case Input rem 100 of
        0 -> 
            integer_to_list(Input div 100);
        Cents when Cents < 10 -> 
            [integer_to_list(Input div 100), $,, $0, Cents + $0 ];
        Cents -> 
            [integer_to_list(Input div 100), $,, integer_to_list(Cents) ]
    end;
format_price(Input, Context) when is_float(Input) ->
    format_price(round(Input * 100), Context);
format_price(Input, Context) when is_function(Input, 0) ->
    format_price(Input(), Context);
format_price(Input, Context) when is_function(Input, 1) ->
    format_price(Input(Context), Context);
format_price(Input, Context) when is_list(Input) ->
    case string:to_integer(Input) of
        {error, _} -> Input;
        {N, _Rest} -> format_price(N, Context)
    end;
format_price(undefined, _Context) ->
    "-".
