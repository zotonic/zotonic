%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%% @doc String related functions

%% Copyright 2009 Marc Worrell
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

-module(z_string).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    trim/1,
    is_string/1,
    line/1,
    to_rootname/1,
    to_name/1,
    to_slug/1,
    to_lower/1,
    to_upper/1,
    replace/3,
	truncate/2,
	truncate/3
]).

-include_lib("include/zotonic.hrl").


%% @doc Remove whitespace at the start and end of the string
%% @todo Check if we want to use a regexp (re) instead, needed for stripping newline, tab etc.
trim(S) -> string:strip(S, both).


%% @doc Check if the variable is a one dimensional list, probably a string
is_string([]) -> 
    true;
is_string([C|Rest]) when is_integer(C) andalso (C >= 32 orelse C == 9 orelse C == 10 orelse C == 12 orelse C == 13) ->
    is_string(Rest);
is_string(_) -> 
    false.


%% @doc Make sure that the string is on one line only, replace control characters with spaces
line(B) when is_binary(B) ->
    line(binary_to_list(B));
line(L) ->
    line1(L, []).
    
    line1([], Acc) ->
        lists:reverse(Acc);
    line1([H|T], Acc) when H < 32 ->
        line1(T, [32 , Acc]);
    line1([H|T], Acc) ->
        line1(T, [H|Acc]).


%% @doc Return a lowercase string for the input
%% @spec to_lower(Value) -> String
to_lower(B) when is_binary(B) ->
    to_lower(binary_to_list(B));
to_lower(A) when is_atom(A) ->
    to_lower(atom_to_list(A));
to_lower(L) when is_list(L) ->
    to_lower(lists:flatten(L), []).

	to_lower([], Acc) -> lists:reverse(Acc);
	to_lower([H|T], Acc) when H >= $A andalso H =< $Z -> to_lower(T, [H+32|Acc]); 
	to_lower("Å"++T, Acc) -> to_lower(T, [165,195|Acc]);
	to_lower("Ä"++T, Acc) -> to_lower(T, [164,195|Acc]);
	to_lower("Á"++T, Acc) -> to_lower(T, [161,195|Acc]);
	to_lower("À"++T, Acc) -> to_lower(T, [160,195|Acc]);
	to_lower("Ë"++T, Acc) -> to_lower(T, [171,195|Acc]);
	to_lower("Ê"++T, Acc) -> to_lower(T, [170,195|Acc]);
	to_lower("É"++T, Acc) -> to_lower(T, [169,195|Acc]);
	to_lower("È"++T, Acc) -> to_lower(T, [168,195|Acc]);
	to_lower("Ï"++T, Acc) -> to_lower(T, [175,195|Acc]);
	to_lower("Î"++T, Acc) -> to_lower(T, [174,195|Acc]);
	to_lower("Í"++T, Acc) -> to_lower(T, [173,195|Acc]);
	to_lower("Ì"++T, Acc) -> to_lower(T, [172,195|Acc]);
	to_lower("Ü"++T, Acc) -> to_lower(T, [188,195|Acc]);
	to_lower("Û"++T, Acc) -> to_lower(T, [187,195|Acc]);
	to_lower("Ú"++T, Acc) -> to_lower(T, [186,195|Acc]);
	to_lower("Ù"++T, Acc) -> to_lower(T, [185,195|Acc]);
	to_lower("Ö"++T, Acc) -> to_lower(T, [182,195|Acc]);
	to_lower("Ô"++T, Acc) -> to_lower(T, [180,195|Acc]);
	to_lower("Ó"++T, Acc) -> to_lower(T, [179,195|Acc]);
	to_lower("Ò"++T, Acc) -> to_lower(T, [178,195|Acc]);
	to_lower("Ø"++T, Acc) -> to_lower(T, [184,195|Acc]);
	to_lower("Ç"++T, Acc) -> to_lower(T, [167,195|Acc]);
	to_lower("Æ"++T, Acc) -> to_lower(T, [166,195|Acc]);
	to_lower("Œ"++T, Acc) -> to_lower(T, [147,197|Acc]);
	to_lower([H|T], Acc) -> to_lower(T, [H|Acc]).


%% @doc Return a uppercase string for the input
%% @spec to_upper(Value) -> String
to_upper(B) when is_binary(B) ->
    to_upper(binary_to_list(B));
to_upper(A) when is_atom(A) ->
    to_upper(atom_to_list(A));
to_upper(L) when is_list(L) ->
    to_upper(lists:flatten(L), []).

	to_upper([], Acc) -> lists:reverse(Acc);
	to_upper([H|T], Acc) when H >= $a andalso H =< $z -> to_upper(T, [H-32|Acc]); 
	to_upper("å"++T, Acc) -> to_upper(T, [133,195|Acc]);
	to_upper("ä"++T, Acc) -> to_upper(T, [132,195|Acc]);
	to_upper("á"++T, Acc) -> to_upper(T, [129,195|Acc]);
	to_upper("à"++T, Acc) -> to_upper(T, [128,195|Acc]);
	to_upper("ë"++T, Acc) -> to_upper(T, [139,195|Acc]);
	to_upper("ê"++T, Acc) -> to_upper(T, [138,195|Acc]);
	to_upper("é"++T, Acc) -> to_upper(T, [137,195|Acc]);
	to_upper("è"++T, Acc) -> to_upper(T, [136,195|Acc]);
	to_upper("ï"++T, Acc) -> to_upper(T, [143,195|Acc]);
	to_upper("Î"++T, Acc) -> to_upper(T, [142,195|Acc]);
	to_upper("í"++T, Acc) -> to_upper(T, [141,195|Acc]);
	to_upper("ì"++T, Acc) -> to_upper(T, [140,195|Acc]);
	to_upper("ü"++T, Acc) -> to_upper(T, [156,195|Acc]);
	to_upper("û"++T, Acc) -> to_upper(T, [155,195|Acc]);
	to_upper("ú"++T, Acc) -> to_upper(T, [154,195|Acc]);
	to_upper("ù"++T, Acc) -> to_upper(T, [153,195|Acc]);
	to_upper("ö"++T, Acc) -> to_upper(T, [150,195|Acc]);
	to_upper("ô"++T, Acc) -> to_upper(T, [148,195|Acc]);
	to_upper("ó"++T, Acc) -> to_upper(T, [147,195|Acc]);
	to_upper("ò"++T, Acc) -> to_upper(T, [146,195|Acc]);
	to_upper("ø"++T, Acc) -> to_upper(T, [152,195|Acc]);
	to_upper("ç"++T, Acc) -> to_upper(T, [135,195|Acc]);
	to_upper("æ"++T, Acc) -> to_upper(T, [134,195|Acc]);
	to_upper("œ"++T, Acc) -> to_upper(T, [146,197|Acc]);
	to_upper([H|T], Acc) -> to_upper(T, [H|Acc]).

%% @doc Filter a filename so that we obtain a basename that is safe to use.
%% @spec to_rootname(string()) -> string()
to_rootname(Filename) ->
    to_slug(filename:rootname(filename:basename(Filename))).


%% @doc Map a string to a slug that can be used in the uri of a page. Same as a name, but then with dashes instead of underscores.
%% @spec to_slug(String) -> String
to_slug(Title) ->
    Slug = to_name(Title),
    [ case C of $_ -> $-; _ -> C end || C <- Slug ].


%% @doc Map a string to a value that can be used as a name or slug. Maps all characters to lowercase and remove non digalpha chars
%% @spec to_name(String) -> String
to_name(Name) when is_binary(Name) ->
    to_name(binary_to_list(Name));
to_name(Name) when is_atom(Name) ->
    to_name(atom_to_list(Name));
to_name(Name) ->
    to_name(Name, [], 0).

to_name([], Acc, _I) ->
    case string:strip(lists:reverse(Acc), both, $_) of
        [] -> "_";
        Name -> Name
    end;
to_name(_, Acc, 80) ->
    to_name([], Acc, 80);
to_name([C|T], Acc, I) when C >= $A andalso C =< $Z ->
    to_name(T, [C+32|Acc], I+1);
to_name([C|T], Acc, I) when (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_ ->
    to_name(T, [C|Acc], I+1);
to_name("ä"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("ë"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ï"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ü"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ö"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ä"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Ë"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("Ï"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Ü"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ö"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("é"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("è"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("É"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("È"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("í"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ì"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Í"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Ì"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ú"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ù"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ú"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ù"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ó"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("ò"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ó"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ò"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("ß"++T, Acc, I) -> to_name(T, [$s,$s|Acc], I+1);
to_name("ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("Ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("€"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ÿ"++T, Acc, I) -> to_name(T, [$i,$j|Acc], I+1);
to_name("@"++T, Acc, I) -> to_name(T, [$_,$t,$a,$_|Acc], I+1);
to_name([_C|T], [$_|_] = Acc, I) ->
    to_name(T, Acc, I+1);
to_name([_C|T], Acc, I) ->
    to_name(T, [$_|Acc], I+1).


%% @doc Replace a string inside another string
%% @copyright 2008 Rusty Klophaus  (Nitrogen, MIT License)
replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
	Length = length(S1),
	case string:substr(String, 1, Length) of 
		S1 -> 
			S2 ++ replace(string:substr(String, Length + 1), S1, S2);
		_ -> 
			[hd(String)|replace(tl(String), S1, S2)]
	end.


%% @doc Truncate a string.  Append the '…' character at the place of break off.
%% @spec truncate(String, int()) -> String
truncate(L, N) ->
	truncate(L, N, "…").

truncate(B, N, Append) when is_binary(B) ->
	truncate(z_convert:to_list(B), N, Append);
truncate(_L, N, _Append) when N =< 0 ->
	[];
truncate(L, N, Append) ->
	truncate(L, N, Append, in_word, [], in_word, []).
	

	truncate([], _, _Append, _LastState, _Last, _AccState, Acc) ->
		lists:reverse(Acc);
	truncate(_, 0, _Append, sentence, Last, _AccState, _Acc) ->
		lists:reverse(Last);
	truncate(_, 0, Append, _, [], _AccState, Acc) ->
		lists:reverse(insert_acc(Append, Acc));
	truncate(_, 0, Append, _LastState, Last, _AccState, _Acc) ->
		lists:reverse(insert_acc(Append, Last));
	truncate([C|Rest], N, Append, LastState, Last, AccState, Acc) 
		when C == $.; C == $!; C == $? ->
			case AccState of
				in_word -> truncate(Rest, N-1, Append, sentence, [C|Acc], sentence, [C|Acc]);
				word    -> truncate(Rest, N-1, Append, sentence, [C|Acc], sentence, [C|Acc]);
				_ 		-> truncate(Rest, N-1, Append, LastState, Last,   sentence, [C|Acc])
			end;
	truncate([C|Rest], N, Append, LastState, Last, AccState, Acc) 
		when C == $;; C == $-; C == $, ->
			case AccState of
				in_word -> truncate(Rest, N-1, Append, sentence,  Acc,  word, [C|Acc]);
				_ 		-> truncate(Rest, N-1, Append, LastState, Last, word, [C|Acc])
			end;
	truncate([C|Rest], N, Append, LastState, Last, AccState, Acc) 
		when C == 32; C == 9; C == 10; C == 13; C == $/; C == $|; C == $(; C == $); C == $" ->
			case AccState of
				in_word -> truncate(Rest, N-1, Append, word, Acc, word, [C|Acc]);
				_       -> truncate(Rest, N-1, Append, LastState, Last, word, [C|Acc])
			end;
	truncate([$&|_]=Input, N, Append, LastState, Last, AccState, Acc) ->
		{Rest1,Acc1} = get_entity(Input,Acc),
		case AccState of
			in_word -> truncate(Rest1, N-1, Append, word, Acc1, word, Acc1);
			_ 		-> truncate(Rest1, N-1, Append, LastState, Last, word, Acc1)
		end;

	%% Overlong encoding: start of a 2-byte sequence, but code point <= 127
	truncate([X,A|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 192, X =< 193 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [A,X|Acc]);
	%% Start of 2-byte sequence
	truncate([X,A|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 194, X =< 223 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [A,X|Acc]);
	%% Start of 3-byte sequence
	truncate([X,A,B|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 224, X =< 239 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [B,A,X|Acc]);
	%% Start of 4-byte sequence
	truncate([X,A,B,C|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 240, X =< 244 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [C,B,A,X|Acc]);
	%% Restricted by RFC 3629: start of 4-byte sequence for codepoint above 10FFFF
	truncate([X,A,B,C|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 245, X =< 247 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [C,B,A,X|Acc]);
	%% Restricted by RFC 3629: start of 5-byte sequence
	truncate([X,A,B,C,D|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 248, X =< 251 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [D,C,B,A,X|Acc]);
	%% Restricted by RFC 3629: start of 6-byte sequence
	truncate([X,A,B,C,D,E|Rest], N, Append, LastState, Last, _AccState, Acc) when X >= 252, X =< 253 ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [E,D,C,B,A,X|Acc]);
	
	%% Any other character
	truncate([C|Rest], N, Append, LastState, Last, _AccState, Acc) ->
		truncate(Rest, N-1, Append, LastState, Last, in_word, [C|Acc]).

	insert_acc([], Acc) ->
		Acc;
	insert_acc([H|T], Acc) ->
		insert_acc(T, [H|Acc]).
	
	get_entity([], Acc) ->
		{[],Acc};
	get_entity([$;|Rest], Acc) ->
		{Rest,[$;|Acc]};
	get_entity([C|Rest], Acc) ->
		get_entity(Rest, [C|Acc]).
	
