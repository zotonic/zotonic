%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Flatten a value to be used a string.

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

-module(filter_flatten_value).
-export([
    flatten_value/2
    ]).

flatten_value(undefined, _Context) ->
    <<>>;
flatten_value(V, _Context) when is_binary(V) ->
    <<>>;
flatten_value([ V | _ ] = Vs, Context) when not is_integer(V) ->
    flatten_list(Vs, Context);
flatten_value(V, Context) when is_list(V) ->
    case z_string:is_string(V) of
        true ->
            z_convert:to_binary(V);
        false ->
            flatten_list(V, Context)
    end;
flatten_value(V, _Context) ->
    z_convert:to_binary(V).


flatten_list(V, Context) ->
    Vs1 = [ flatten_value(X, Context) || X <- V ],
    iolist_to_binary( lists:join($,, Vs1) ).

