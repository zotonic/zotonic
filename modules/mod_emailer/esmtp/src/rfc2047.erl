%% Encode a string according to RFC 2047 using quoted-printable.
%% Assumes UTF-8 as the character set.
%%
%% @copyright 2009 Marc Worrell

-module(rfc2047).
-author("Marc Worrell <marc@worrell.nl>").

-export([encode/1]).


encode(B) when is_binary(B) ->
	encode(binary_to_list(B));
encode([]) -> 
	[];
encode(Text) ->
	"=?UTF-8?Q?" ++ encode(Text, [], 0) ++ "?=".
	

encode([], Acc, _WordLen) ->
	lists:reverse(Acc);
encode(T, Acc, WordLen) when WordLen >= 55 ->
	%% Make sure that the individual encoded words are not longer than 76 chars (including charset etc)
	encode(T, [$?,$Q,$?,$8,$-,$F,$T,$U,$?,$=,32,10,13,$=,$?|Acc], 0);
encode([C|T], Acc, WordLen) when C > 32 andalso C < 127 andalso C /= 32 andalso C /= $? andalso C /= $_ andalso C /= $= ->
	encode(T, [C|Acc], WordLen+1);
encode([C|T], Acc, WordLen) ->
	encode(T, [hex(C rem 16), hex(C div 16), $= | Acc], WordLen+4).
	
hex(N) when N >= 10 -> N + $A - 10;
hex(N) -> N + $0.
