%% @author Marc Worrell
%% @copyright 2012 Marc Worrell
%% @doc JSON support routines.

%% Copyright 2012 Marc Worrell
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

-module(z_json).
-include("zotonic.hrl").

-export([
    encode/1,
    decode/1,
    json_escape/1
]).

%% @doc Encode an Erlang term to JSON.
-spec encode(binary() | map()) -> binary().
encode(Erlang) ->
    jsx:encode(Erlang).

%% @doc Decode a JSON binary to an Erlang term.
-spec decode(binary()) -> jsx:json_term().
decode(Json) ->
    jsx:decode(Json, [return_maps]).

%%% ESCAPE JSON %%%

%% @doc JSON escape for safe quoting of JSON strings. Subtly different
%% from JS escape, see http://www.json.org/
json_escape(undefined) -> [];
json_escape([]) -> [];
json_escape(<<>>) -> [];
json_escape(Value) when is_integer(Value) -> integer_to_list(Value);
json_escape(Value) when is_atom(Value) -> json_escape(atom_to_list(Value), []);
json_escape(Value) when is_binary(Value) -> json_escape(binary_to_list(Value), []);
json_escape(Value) -> json_escape(Value, []).

json_escape([], Acc) -> lists:reverse(Acc);
json_escape([$" |T], Acc) -> json_escape(T, [$" ,$\\|Acc]);
json_escape([$\\|T], Acc) -> json_escape(T, [$\\,$\\|Acc]);
json_escape([$/ |T], Acc) -> json_escape(T, [$/ ,$\\|Acc]);
json_escape([$\b|T], Acc) -> json_escape(T, [$b ,$\\|Acc]);
json_escape([$\f|T], Acc) -> json_escape(T, [$f ,$\\|Acc]);
json_escape([$\n|T], Acc) -> json_escape(T, [$n ,$\\|Acc]);
json_escape([$\r|T], Acc) -> json_escape(T, [$r ,$\\|Acc]);
json_escape([$\t|T], Acc) -> json_escape(T, [$t ,$\\|Acc]);
json_escape([H|T], Acc) when is_integer(H) ->
    json_escape(T, [H|Acc]);
json_escape([H|T], Acc) ->
    H1 = json_escape(H),
    json_escape(T, [H1|Acc]).
