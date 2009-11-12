%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.
%%% 20090311 Marc Worrell - Added support for encoding terms and lists of terms in bytea values

-module(pgsql_binary).

-export([encode/3, decode/3, supports/1]).

-include_lib("zotonic.hrl").

-define(int32, 1/big-signed-unit:32).
-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).


encode(_Any, null, _UseIntDT)  -> <<-1:?int32>>;
encode(_Any, undefined, _UseIntDT)  -> <<-1:?int32>>;
encode(bool, <<1>>, _UseIntDT) -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, <<>>, _UseIntDT)  -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(bool, true, _UseIntDT)  -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, false, _UseIntDT) -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(int2, N, UseIntDT) when is_binary(N); is_list(N) -> encode(int2, z_convert:to_integer(N), UseIntDT);
encode(int4, N, UseIntDT) when is_binary(N); is_list(N) -> encode(int4, z_convert:to_integer(N), UseIntDT);
encode(int8, N, UseIntDT) when is_binary(N); is_list(N) -> encode(int8, z_convert:to_integer(N), UseIntDT);
encode(int2, N, _UseIntDT)     -> <<2:?int32, N:1/big-signed-unit:16>>;
encode(int4, N, _UseIntDT)     -> <<4:?int32, N:1/big-signed-unit:32>>;
encode(int8, N, _UseIntDT)     -> <<8:?int32, N:1/big-signed-unit:64>>;
encode(float4, N, _UseIntDT)   -> <<4:?int32, N:1/big-float-unit:32>>;
encode(float8, N, _UseIntDT)   -> <<8:?int32, N:1/big-float-unit:64>>;
encode(bpchar, C, _UseIntDT) when is_integer(C) -> <<1:?int32, C:1/big-unsigned-unit:8>>;
encode(bpchar, B, _UseIntDT) when is_binary(B)  -> <<(byte_size(B)):?int32, B/binary>>;
encode(Type, B, UseIntDT) when Type == time; Type == timetz          -> pgsql_datetime:encode(Type, B, UseIntDT);
encode(Type, B, UseIntDT) when Type == date; Type == timestamp       -> pgsql_datetime:encode(Type, B, UseIntDT);
encode(Type, B, UseIntDT) when Type == timestamptz; Type == interval -> pgsql_datetime:encode(Type, B, UseIntDT);
encode(bytea, B, _UseIntDT) when is_binary(B)   -> <<(byte_size(B)):?int32, B/binary>>;
encode(text, B, _UseIntDT) when is_binary(B)    -> <<(byte_size(B)):?int32, B/binary>>;
encode(varchar, B, _UseIntDT) when is_binary(B) -> <<(byte_size(B)):?int32, B/binary>>;
encode(bytea, T, UseIntDT) when is_tuple(T)   -> 
    B = term_to_binary(T),
    encode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, UseIntDT);
encode(bytea, [T|_Rest]=L, UseIntDT) when is_tuple(T)   -> 
    B = term_to_binary(L),
    encode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, UseIntDT);
encode(Type, A, UseIntDT) when is_atom(A)      -> encode(Type, atom_to_list(A), UseIntDT);
encode(Type, L, UseIntDT) when is_list(L)      -> encode(Type, iolist_to_binary(L), UseIntDT);
encode(_Type, _Value, _UseIntDT)                -> {error, unsupported}.

decode(bool, <<1:1/big-signed-unit:8>>, _UseIntDT)     -> true;
decode(bool, <<0:1/big-signed-unit:8>>, _UseIntDT)     -> false;
decode(bool, <<"t">>, _UseIntDT) -> true;
decode(bool, <<"f">>, _UseIntDT) -> false;
decode(bpchar, <<C:1/big-unsigned-unit:8>>, _UseIntDT) -> C;
decode(int2, <<N:1/big-signed-unit:16>>, _UseIntDT)    -> N;
decode(int4, <<N:1/big-signed-unit:32>>, _UseIntDT)    -> N;
decode(int8, <<N:1/big-signed-unit:64>>, _UseIntDT)    -> N;
decode(float4, <<N:1/big-float-unit:32>>, _UseIntDT)   -> N;
decode(float8, <<N:1/big-float-unit:64>>, _UseIntDT)   -> N;
decode(record, <<_:?int32, Rest/binary>>, UseIntDT)   -> list_to_tuple(decode_record(Rest, UseIntDT, []));
decode(Type, B, UseIntDT) when Type == time; Type == timetz          -> pgsql_datetime:decode(Type, B, UseIntDT);
decode(Type, B, UseIntDT) when Type == date; Type == timestamp       -> pgsql_datetime:decode(Type, B, UseIntDT);
decode(Type, B, UseIntDT) when Type == timestamptz; Type == interval -> pgsql_datetime:decode(Type, B, UseIntDT);
decode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, _UseIntDT) -> binary_to_term(B);
decode(_Other, Bin, _UseIntDT) -> Bin.

decode_record(<<>>, _UseIntDT, Acc) ->
    lists:reverse(Acc);
decode_record(<<_Type:?int32, -1:?int32, Rest/binary>>, UseIntDT, Acc) ->
    decode_record(Rest, UseIntDT, [undefined | Acc]);
decode_record(<<Type:?int32, Len:?int32, Value:Len/binary, Rest/binary>>, UseIntDT, Acc) ->
    Value2 = decode(pgsql_types:oid2type(Type), Value, UseIntDT),
    decode_record(Rest, UseIntDT, [Value2 | Acc]).

supports(bool)    -> true;
supports(bpchar)  -> true;
supports(int2)    -> true;
supports(int4)    -> true;
supports(int8)    -> true;
supports(float4)  -> true;
supports(float8)  -> true;
supports(bytea)   -> true;
supports(text)    -> true;
supports(varchar) -> true;
supports(record)  -> true;
supports(date)    -> true;
supports(time)    -> true;
supports(timetz)  -> true;
supports(timestamp)   -> true;
supports(timestamptz) -> true;
supports(interval)    -> true;
supports(_Type)       -> false.
