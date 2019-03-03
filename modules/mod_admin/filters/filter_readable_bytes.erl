%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc Convert bytes into a human readable value.

%% Copyright 2019 Maas-Maarten Zeeman
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

-module(filter_readable_bytes).
-export([
    readable_bytes/2
]).

readable_bytes(Bytes, _Context) ->
    readable(floor(math:log(Bytes) / math:log(1000)), Bytes).

%%
%% Helpers
%%

readable(0, Bytes) ->
    [z_convert:to_string(Bytes), unit(0)]; 
readable(Unit, Bytes) ->
    [round_digits(Bytes/math:pow(1000, Unit)), unit(Unit)].

unit(0) -> <<" B">>;
unit(1) -> <<" kB">>;
unit(2) -> <<" MB">>;
unit(3) -> <<" GB">>;
unit(4) -> <<" TB">>;
unit(5) -> <<" PB">>.

round_digits(N) -> float_to_list(N, [{decimals, 2}]).



