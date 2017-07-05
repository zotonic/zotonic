%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Take maximum value.

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

-module(filter_max).
-export([max/3]).

max(undefined, _Arg, _Context) ->
    undefined;
max(Value, undefined, _Context) ->
    Value;
max({trans, _} = Tr, Arg, Context) ->
    max(z_trans:lookup_fallback(Tr, Context), Arg, Context);
max(Value, {trans, _} = Tr, Context) ->
    max(Value, z_trans:lookup_fallback(Tr, Context), Context);
max(Value, Arg, _Context) ->
    case Value > Arg of
        true -> Value;
        false -> Arg
    end.


