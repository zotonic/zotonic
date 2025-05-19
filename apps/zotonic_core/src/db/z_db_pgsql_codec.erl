%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Postgresql zotonic codec
%%
%% These are conversion routines between how z_db expects values and how epgsl expects them.
%%
%% Notable differences:
%% - Input values {term, ...} (use the ?DB_PROPS(...) macro!) are term_to_binary encoded and decoded
%% - date/datetimes have a floating-point second argument in epgsql, in Zotonic they don't.
%% @end

%% Copyright 2025 Arjan Scherpenisse, Maas-Maarten Zeeman
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

-module(z_db_pgsql_codec).
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

%% 4-byte magic number used as a prefix to serialized Erlang terms.
%% Helps identify and distinguish term binaries in raw byte streams.
-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).

init(Term, Sock) ->
    %% The datetime codec has to check what time formatting is used
    epgsql_codec_datetime:init(Term, Sock).

names() ->
    [jsonb, bytea, time, timetz, date, timestamp, timestamptz].

% bytea
encode({term, Term}, bytea, State) ->
    B = term_to_binary(Term),
    <<?TERM_MAGIC_NUMBER, B/binary>>,
    epgsql_codec_text:encode(<<?TERM_MAGIC_NUMBER, B/binary>>, bytea, State);
encode(Cell, bytea, State) ->
    epgsql_codec_text:encode(Cell, bytea, State);
% jsonb
encode({term_json, Term}, jsonb, State) ->
    epgsql_codec_json:encode(jsxrecord:encode(Term), jsonb, []);
encode(Term, jsonb, State) ->
    epgsql_codec_json:encode(Term, jsonb, []);
% datetime types
encode(Cell, TypeName, State) ->
    epgsql_codec_datetime:encode(Cell, TypeName, State).

% bytea
decode(Cell, bytea, _State) ->
    decode_value(epgsql_codec_text:decode(Cell, bytea, []));
% jsonb
decode(Cell, jsonb, _State) ->
    decode_value(epgsql_codec_json:decode(Cell, jsonb, []));
% datetime types
decode(Cell, TypeName, State) ->
    decode_value(epgsql_codec_datetime:decode(Cell, TypeName, State)).

decode_text(Cell, bytea, _State) ->
    epgsql_codec_text:decode_text(Cell, bytea, []);
decode_text(Cell, jsonb, _State) ->
    epgsql_codec_jsong:decode_text(Cell, jsonb, []);
decode_text(Cell, TypeName, State) ->
    epgsql_codec_datetime:decode_text(Cell, TypeName, State).

decode_value(<<?TERM_MAGIC_NUMBER, B/binary>>) ->
    binary_to_term(B);
decode_value({H, M, S}) when is_float(S) ->
    {H, M, trunc(S)};
decode_value({{Y, Mm, D}, {H, M, S}}) when is_float(S) ->
    {{Y, Mm, D}, {H, M, trunc(S)}};
decode_value(V) ->
    V.
