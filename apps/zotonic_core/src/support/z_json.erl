%% @author Marc Worrell
%% @copyright 2012-2023 Marc Worrell
%% @doc JSON support routines.
%% @end

%% Copyright 2012-2023 Marc Worrell
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

%% @doc Encode an Erlang term to JSON. Ensure all '/' characters in the JSON
%% are escaped for safe embedding the generated JSON in HTML text. Malformed
%% UTF-8 is replaced with U+FFFD without changing valid text or stored data.
-spec encode(binary() | map() | list() | number() | boolean() | null | undefined) -> binary().
encode(Erlang) ->
    Json = try jsxrecord:encode(Erlang)
    catch
        error:{invalid_byte, _} -> jsxrecord:encode(repair_utf8(Erlang));
        error:unexpected_end -> jsxrecord:encode(repair_utf8(Erlang))
    end,
    binary:replace(Json, <<"/">>, <<"\\/">>, [ global ]).

%% Keep the normal encoder fast; only walk the term after a UTF-8 error.
%% Tuples retain their shape so jsxrecord can still encode records and dates.
repair_utf8(Bin) when is_binary(Bin) ->
    repair_utf8_binary(Bin, []);
repair_utf8(Map) when is_map(Map) ->
    maps:from_list([{repair_utf8(K), repair_utf8(V)} || {K, V} <- maps:to_list(Map)]);
repair_utf8(List) when is_list(List) ->
    [repair_utf8(V) || V <- List];
repair_utf8(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([repair_utf8(V) || V <- tuple_to_list(Tuple)]);
repair_utf8(Value) ->
    Value.

repair_utf8_binary(Bin, Acc) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Valid when is_binary(Valid) ->
            iolist_to_binary(lists:reverse([Valid | Acc]));
        {error, Valid, <<_, Rest/binary>>} ->
            repair_utf8_binary(Rest, [<<16#fffd/utf8>>, Valid | Acc]);
        {incomplete, Valid, _Rest} ->
            iolist_to_binary(lists:reverse([<<16#fffd/utf8>>, Valid | Acc]))
    end.

%% @doc Decode a JSON binary to an Erlang term.
-spec decode(binary() | undefined) -> term().
decode(undefined) ->
    undefined;
decode(Json) ->
    jsxrecord:decode(Json).

%%% ESCAPE JSON %%%

%% @doc JSON escape for safe quoting of JSON strings. Subtly different
%% from JS escape, see http://www.json.org/
-spec json_escape(iodata() | undefined) -> iodata().
json_escape(undefined) -> <<>>;
json_escape(Value) when is_binary(Value) -> json_escape_b(Value, <<>>);
json_escape(Value) when is_integer(Value) -> integer_to_list(Value);
json_escape(Value) when is_atom(Value) -> json_escape(atom_to_list(Value), []);
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

json_escape_b(<<>>, Acc) -> Acc;
json_escape_b(<<$", T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $">>);
json_escape_b(<<$\\,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $\\>>);
json_escape_b(<<$/, T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $/>>);
json_escape_b(<<$\b,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $b>>);
json_escape_b(<<$\f,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $f>>);
json_escape_b(<<$\n,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $n>>);
json_escape_b(<<$\r,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $r>>);
json_escape_b(<<$\t,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, $\\, $t>>);
json_escape_b(<<C/utf8,T/binary>>, Acc) -> json_escape_b(T, <<Acc/binary, C/utf8>>).
