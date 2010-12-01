%% @author Marc Worrell <marc@worrell.nl>
%% @data 2010-02-12
%% @copyright 2010 Marc Worrell
%% @desc Encode data to quoted printable strings.

-module(z_quoted_printable).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    encode/1
]).


%% @doc Encode a string as quoted printable.
%% @spec encode(iolist()) -> binary()
encode(L) when is_list(L) ->
    encode(iolist_to_binary(L));
encode(B) when is_binary(B) ->
    encode(B, 0, <<>>).
    
    encode(<<>>, _, Acc) ->
        Acc;
    encode(B, Len, Acc) when Len >= 72 ->
        encode(B, 0, <<Acc/binary, $=, 13, 10>>);
    encode(<<C,Rest/binary>>, Len, Acc) when C < 32 orelse C >= 127 orelse C =:= $= orelse C =:= $. ->
        H1 = to_hex(C div 16),
        H2 = to_hex(C rem 16),
        encode(Rest, Len+3, <<Acc/binary, $=, H1, H2>>);
    encode(<<C,Rest/binary>>, Len, Acc) ->
        encode(Rest, Len+1, <<Acc/binary, C>>).


to_hex(C) when C < 10 -> 
    C + $0;
to_hex(C) -> 
    C - 10 + $A.
