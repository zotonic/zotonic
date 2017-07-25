%% Encode a string according to RFC 2047 using quoted-printable.
%% Assumes UTF-8 as the character set.
%%
%% @copyright 2009 Marc Worrell

-module(rfc2047).
-author("Marc Worrell <marc@worrell.nl>").

-export([encode/1, decode/1]).


-spec encode(string()|binary()) -> binary().
encode(B) when is_list(B) ->
	encode(list_to_binary(B));
encode(<<>>) ->
	<<>>;
encode(Text) ->
    encode(Text, Text).

%% Don't escape when all characters are ASCII printable
encode(<<>>, Text) ->
    Text;
encode(<<H,T/binary>>, Text) when H >= 32 andalso H =< 126 andalso H /= $= ->
    encode(T, Text);
encode(_, Text) ->
    <<"=?UTF-8?Q?", (encode(Text, <<>>, 0))/binary, "?=">>.

encode(<<>>, Acc, _WordLen) ->
    Acc;
encode(T, Acc, WordLen) when WordLen >= 55 ->
    %% Make sure that the individual encoded words are not longer than 76 chars (including charset etc)
    encode(T, <<Acc/binary,"?=\r\n =?UTF-8?Q?">>, 0);
encode(<<C, T/binary>>, Acc, WordLen) when C > 32 andalso C < 127 andalso C /= 32
    andalso C /= $? andalso C /= $_ andalso C /= $= andalso C /= $. ->
    encode(T, <<Acc/binary, C>>, WordLen+1);
encode(<<C, T/binary>>, Acc, WordLen) ->
    C2 = hex(C rem 16),
    C1 = hex(C div 16),
    encode(T, <<Acc/binary, $=, C1, C2>>, WordLen+3).


-spec decode(string()|binary()) -> binary().
decode(B) when is_list(B) ->
    decode(list_to_binary(B));
decode(Text) ->
    decode(Text, in_text, <<>>).

decode(<<>>, _, Acc) ->
    Acc;
decode(<<"=?UTF-8?Q?", T/binary>>, in_text, Acc) ->
    decode(T, in_utf8, Acc);
decode(<<"?= \r\n", T/binary>>, in_utf8, Acc) ->
    decode(T, in_text, Acc);
decode(<<"?=", T/binary>>, in_utf8, Acc) ->
    decode(T, in_text, Acc);
decode(<<$=,C1,C2, T/binary>>, in_utf8, Acc) ->
    C = unhex(C1)*16 + unhex(C2),
    decode(T, in_utf8, <<Acc/binary, C>>);
decode(<<H,T/binary>>, State, Acc) ->
    decode(T, State, <<Acc/binary, H>>).

hex(N) when N >= 10 -> N + $A - 10;
hex(N) -> N + $0.

unhex(C) when C >= $a ->
    C - $a + 10;
unhex(C) when C >= $A ->
    C - $A + 10;
unhex(C) ->
    C - $0.

