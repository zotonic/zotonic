%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Convert a value from json
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_from_json).
-export([from_json/2]).

%% @doc Decode a JSON string to a term. If the decode fails then 'undefined' is returned.
-spec from_json( term(), z:context() ) -> term().
from_json(Value, _Context) when is_binary(Value) ->
    try
        z_json:decode(Value)
    catch
        _:_ -> undefined
    end;
from_json(Value, _Context) ->
    Value.
