
-module(z_db_pgsql_codec).
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).

-include("zotonic.hrl").

init(Term, Sock) ->
    epgsql_codec_datetime:init(Term, Sock).

names() ->
    [bytea, time, timetz, date, timestamp, timestamptz].

encode(Cell, bytea, State) ->
    epgsql_codec_text:encode(Cell, bytea, State);
encode(Cell, TypeName, State) ->
    epgsql_codec_datetime:encode(Cell, TypeName, State).

decode(Cell, bytea, State) ->
    decode_value(epgsql_codec_text:decode(Cell, bytea, State));
decode(Cell, TypeName, State) ->
    decode_value(epgsql_codec_datetime:decode(Cell, TypeName, State)).

decode_text(Cell, bytea, State) ->
    epgsql_codec_text:decode_text(Cell, bytea, State);
decode_text(Cell, TypeName, State) ->
    epgsql_codec_datetime:decode_text(Cell, TypeName, State).

decode_value(<<?TERM_MAGIC_NUMBER, B/binary>>) ->
    binary_to_term(B);
decode_value({H,M,S}) when is_float(S) ->
    {H,M,trunc(S)};
decode_value({{Y,Mm,D},{H,M,S}}) when is_float(S) ->
    {{Y,Mm,D},{H,M,trunc(S)}};
decode_value(V) ->
    V.
