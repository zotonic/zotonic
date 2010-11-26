%% Encode a string according to RFC 2047 using quoted-printable.
%% Assumes UTF-8 as the character set.
%%
%% @copyright 2009 Marc Worrell

-module(rfc2047).
-author("Marc Worrell <marc@worrell.nl>").

-export([encode/1, decode/1]).


encode(B) when is_binary(B) ->
	encode(binary_to_list(B));
encode([]) -> 
	[];
encode(Text) ->
    encode(Text, Text).

    %% Don't escape when all characters are ASCII printable
    encode([], Text) ->
        Text;
    encode([H|T], Text) when H >= 32 andalso H =< 126 andalso H /= $= ->
        encode(T, Text);
    encode(_, Text) ->
        "=?UTF-8?Q?" ++ encode(Text, [], 0) ++ "?=".

encode([], Acc, _WordLen) ->
    lists:reverse(Acc);
encode(T, Acc, WordLen) when WordLen >= 55 ->
    %% Make sure that the individual encoded words are not longer than 76 chars (including charset etc)
    encode(T, [$?,$Q,$?,$8,$-,$F,$T,$U,$?,$=,32,10,13,$=,$?|Acc], 0);
encode([C|T], Acc, WordLen) when C > 32 andalso C < 127 andalso C /= 32 
    andalso C /= $? andalso C /= $_ andalso C /= $= andalso C /= $. ->
    encode(T, [C|Acc], WordLen+1);
encode([C|T], Acc, WordLen) ->
    encode(T, [hex(C rem 16), hex(C div 16), $= | Acc], WordLen+3).

decode(B) when is_binary(B) ->
    decode(binary_to_list(B));
decode(Text) ->
    decode(Text, in_text, []).

decode([], _, Acc) ->
    lists:reverse(Acc);    
decode("=?UTF-8?Q?" ++ T, in_text, Acc) ->
    decode(T, in_utf8, Acc);
decode("?= \r\n" ++ T, in_utf8, Acc) ->
    decode(T, in_text, Acc);
decode("?=" ++ T, in_utf8, Acc) ->
    decode(T, in_text, Acc);
decode([$=,C1,C2|T], in_utf8, Acc) ->
    decode(T, in_utf8, [unhex(C1)*16+unhex(C2)|Acc]);
decode([H|T], State, Acc) ->
    decode(T, State, [H|Acc]).

hex(N) when N >= 10 -> N + $A - 10;
hex(N) -> N + $0.

unhex(C) when C >= $a ->
    C - $a + 10;
unhex(C) when C >= $A ->
    C - $A + 10;
unhex(C) ->
    C - $0.

