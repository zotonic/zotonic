% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% Date: 2009-04-26
%% @doc String related functions

%% @todo Check valid chars for filenames, allow chinese, japanese, etc?
%% CJK Unified Ideographs Extension A: Range: 3400-4DBF
%% CJK Unified Ideographs: Range: 4E00-9FAF
%% Kangxi Radicals: Range 2F00-2FDF
%% See also: http://www.utf8-chartable.de/

%% Copyright 2009-2010 Marc Worrell
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
    trim_left/1,
    trim_right/1,
    trim/2,
    trim_left/2,
    trim_right/2,
    trim_left_func/2,
    is_string/1,
    first_char/1,
    last_char/1,
    unquote/1,
    unquote/2,
    nospaces/1,
    line/1,
    to_rootname/1,
    to_name/1,
    to_slug/1,
    to_lower/1,
    to_upper/1,
    replace/3,
    sanitize_utf8/1,
    truncate/2,
    truncate/3,
    truncatewords/2,
    truncatewords/3,
    split_lines/1,
    escape_ical/1,
    starts_with/2,
    ends_with/2,
    contains/2,
    split/2,
    test/0
]).

-include_lib("include/zotonic.hrl").


%% @doc Remove whitespace at the start and end of the string
trim(B) when is_binary(B) ->
	trim_right(trim_left(B));
trim(L) when is_list(L) ->
	binary_to_list(trim(iolist_to_binary(L))).

%% @doc Remove all occurences of a character at the start and end of a string.
trim(B, Char) when is_binary(B) ->
	trim_right(trim_left(B, Char), Char);
trim(L, Char) when is_list(L) ->
	binary_to_list(trim(iolist_to_binary(L), Char)).


%% @doc Remove whitespace at the start the string
trim_left(S) ->
    trim_left_func(S, fun(C) -> C =< 32 end).

%% @doc Remove all occurences of a char at the start of a string
trim_left(S, Char) ->
    trim_left_func(S, fun(C) -> C == Char end).


trim_left_func(<<Char, Rest/binary>> = Bin, F) ->
    case F(Char) of
        true -> trim_left_func(Rest, F);
        false -> Bin
    end;
trim_left_func([Char|Rest] = L, F) when is_integer(Char) ->
    case F(Char) of
        true -> trim_left(Rest, F);
        false -> L
    end;
trim_left_func([L|Rest], F) when is_list(L); is_binary(L) ->
    case trim_left_func(L, F) of
        [] -> trim_left_func(Rest, F);
        <<>> -> trim_left_func(Rest, F);
        Other -> [Other|Rest]
    end;
trim_left_func(Other, _F) ->
    Other.

    
	
%% @doc Remove whitespace at the end of the string
trim_right(B) when is_binary(B) ->
	trim_right(B, <<>>, <<>>);
trim_right(L) ->
	binary_to_list(trim_right(iolist_to_binary(L))).

	trim_right(<<C, Rest/binary>>, WS, Acc) ->
		case C of
			W when W =< 32 -> trim_right(Rest, <<WS/binary, C>>, Acc);
			_ -> trim_right(Rest, <<>>, <<Acc/binary, WS/binary, C>>)
		end;
	trim_right(<<>>, _WS, Acc) ->
		Acc.

%% @doc Remove all occurences of a char at the end of the string
trim_right(B, Char) when is_binary(B) ->
	trim_right(B, Char, <<>>, <<>>);
trim_right(L, Char) ->
	binary_to_list(trim_right(iolist_to_binary(L), Char)).

	trim_right(<<C, Rest/binary>>, Char, WS, Acc) ->
		case C of
			Char -> trim_right(Rest, Char, <<WS/binary, C>>, Acc);
			_ -> trim_right(Rest, Char, <<>>, <<Acc/binary, WS/binary, C>>)
		end;
	trim_right(<<>>, _Char, _WS, Acc) ->
		Acc.

%% @doc Check if the variable is a one dimensional list, probably a string
is_string([]) -> 
    true;
is_string([C|Rest]) when 
		is_integer(C)
		andalso C =< 255
		andalso (C >= 32 orelse C == 9 orelse C == 10 orelse C == 12 orelse C == 13) ->
    is_string(Rest);
is_string(_) -> 
    false.


%% @doc Return the first character of a string.
%% @todo Make this UTF-8 safe
first_char([]) -> undefined;
first_char([H|T]) when is_integer(H) ->
    truncate([H|T], 1, "");
first_char(<<>>) -> undefined;
first_char(<<C, _/binary>>) -> C.


%% @doc Return the last character of a string
last_char([]) -> undefined;
last_char([C]) -> C;
last_char([_|R]) -> last_char(R);
last_char(<<>>) -> undefined;
last_char(<<C>>) -> C;
last_char(<<_, R/binary>>) -> last_char(R).


%% @doc Remove the first and last char if they are double quotes.
unquote(S) ->
    unquote(S, $").

unquote(S, Q) ->
    case S of
        <<Q, R/binary>> -> unquote1(R, <<>>, Q, S);
        [Q|R] -> unquote1(R, [], Q, S);
        _ -> S
    end.
    
    unquote1([], _Acc, _Q, S) -> S;
    unquote1([Q], Acc, Q, _S) -> lists:reverse(Acc);
    unquote1([H|T], Acc, Q, S) -> unquote1(T, [H|Acc], Q, S);

    unquote1(<<>>, _Acc, _Q, S) -> S;
    unquote1(<<Q>>, Acc, Q, _S) -> Acc;
    unquote1(<<C,R/binary>>, Acc, Q, S) -> unquote1(R, <<Acc/binary, C>>, Q, S).


%% @doc Remove all spaces and control characters from a string.
nospaces(B) when is_binary(B) ->
    nospaces(binary_to_list(B));
nospaces(L) ->
    nospaces(L, []).

nospaces([], Acc) ->
    lists:reverse(Acc);
nospaces([C|Rest], Acc) when C =< 32 ->
    nospaces(Rest, Acc);
nospaces([C|Rest], Acc) ->
    nospaces(Rest, [C|Acc]).



%% @doc Make sure that the string is on one line only, replace control characters with spaces
line(B) when is_binary(B) ->
    line(binary_to_list(B));
line(L) ->
    line1(L, []).
    
    line1([], Acc) ->
        lists:reverse(Acc);
    line1([H|T], Acc) when H < 32 ->
        line1(T, [32|Acc]);
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
    to_lower([B|T], Acc) when is_binary(B) -> to_lower(binary_to_list(B)++T, Acc);
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
	% Cyrillic support
	to_lower("А"++T, Acc) -> to_lower(T, [176,208|Acc]);
	to_lower("Б"++T, Acc) -> to_lower(T, [177,208|Acc]);
	to_lower("В"++T, Acc) -> to_lower(T, [178,208|Acc]);
	to_lower("Г"++T, Acc) -> to_lower(T, [179,208|Acc]);
	to_lower("Д"++T, Acc) -> to_lower(T, [180,208|Acc]);
	to_lower("Е"++T, Acc) -> to_lower(T, [181,208|Acc]);
	to_lower("Ё"++T, Acc) -> to_lower(T, [145,209|Acc]);
	to_lower("Ж"++T, Acc) -> to_lower(T, [182,208|Acc]);
	to_lower("З"++T, Acc) -> to_lower(T, [183,208|Acc]);
	to_lower("И"++T, Acc) -> to_lower(T, [184,208|Acc]);
	to_lower("Й"++T, Acc) -> to_lower(T, [185,208|Acc]);
	to_lower("К"++T, Acc) -> to_lower(T, [186,208|Acc]);
	to_lower("Л"++T, Acc) -> to_lower(T, [187,208|Acc]);
	to_lower("М"++T, Acc) -> to_lower(T, [188,208|Acc]);
	to_lower("Н"++T, Acc) -> to_lower(T, [189,208|Acc]);
	to_lower("О"++T, Acc) -> to_lower(T, [190,208|Acc]);
	to_lower("П"++T, Acc) -> to_lower(T, [191,208|Acc]);
	to_lower("Р"++T, Acc) -> to_lower(T, [128,209|Acc]);
	to_lower("С"++T, Acc) -> to_lower(T, [129,209|Acc]);
	to_lower("Т"++T, Acc) -> to_lower(T, [130,209|Acc]);
	to_lower("У"++T, Acc) -> to_lower(T, [131,209|Acc]);
	to_lower("Ф"++T, Acc) -> to_lower(T, [132,209|Acc]);
	to_lower("Х"++T, Acc) -> to_lower(T, [133,209|Acc]);
	to_lower("Ц"++T, Acc) -> to_lower(T, [134,209|Acc]);
	to_lower("Ч"++T, Acc) -> to_lower(T, [135,209|Acc]);
	to_lower("Ш"++T, Acc) -> to_lower(T, [136,209|Acc]);
	to_lower("Щ"++T, Acc) -> to_lower(T, [137,209|Acc]);
	to_lower("Ъ"++T, Acc) -> to_lower(T, [138,209|Acc]);
	to_lower("Ы"++T, Acc) -> to_lower(T, [139,209|Acc]);
	to_lower("Ь"++T, Acc) -> to_lower(T, [140,209|Acc]);
	to_lower("Э"++T, Acc) -> to_lower(T, [141,209|Acc]);
	to_lower("Ю"++T, Acc) -> to_lower(T, [142,209|Acc]);
	to_lower("Я"++T, Acc) -> to_lower(T, [143,209|Acc]);
	% Extra Ukrainian characters
	to_lower("Ґ"++T, Acc) -> to_lower(T, [145,210|Acc]);
	to_lower("Ї"++T, Acc) -> to_lower(T, [151,209|Acc]);
	to_lower("І"++T, Acc) -> to_lower(T, [150,209|Acc]);
	to_lower("Є"++T, Acc) -> to_lower(T, [148,209|Acc]);
	% Polish support
	to_lower("Ą"++T, Acc) -> to_lower(T, [133,196|Acc]);
	to_lower("Ę"++T, Acc) -> to_lower(T, [153,196|Acc]);
	to_lower("Ć"++T, Acc) -> to_lower(T, [135,196|Acc]);
	to_lower("Ł"++T, Acc) -> to_lower(T, [130,197|Acc]);
	to_lower("Ń"++T, Acc) -> to_lower(T, [132,197|Acc]);
	to_lower("Ś"++T, Acc) -> to_lower(T, [155,197|Acc]);
	to_lower("Ź"++T, Acc) -> to_lower(T, [186,197|Acc]);
	to_lower("Ż"++T, Acc) -> to_lower(T, [188,197|Acc]);
    % Turkish support
	to_lower("Ş"++T, Acc) -> to_lower(T, [159,197|Acc]);
	to_lower("Ğ"++T, Acc) -> to_lower(T, [159,196|Acc]);
	to_lower("İ"++T, Acc) -> to_lower(T, [177,196|Acc]);
	% Other characters are taken as-is
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
    to_upper([B|T], Acc) when is_binary(B) -> to_upper(binary_to_list(B)++T, Acc);
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
	% Cyrillic support
	to_upper("а"++T, Acc) -> to_upper(T, [144,208|Acc]);
	to_upper("б"++T, Acc) -> to_upper(T, [145,208|Acc]);
	to_upper("в"++T, Acc) -> to_upper(T, [146,208|Acc]);
	to_upper("г"++T, Acc) -> to_upper(T, [147,208|Acc]);
	to_upper("д"++T, Acc) -> to_upper(T, [148,208|Acc]);
	to_upper("е"++T, Acc) -> to_upper(T, [149,208|Acc]);
	to_upper("ё"++T, Acc) -> to_upper(T, [129,208|Acc]);
	to_upper("ж"++T, Acc) -> to_upper(T, [150,208|Acc]);
	to_upper("з"++T, Acc) -> to_upper(T, [151,208|Acc]);
	to_upper("и"++T, Acc) -> to_upper(T, [152,208|Acc]);
	to_upper("й"++T, Acc) -> to_upper(T, [153,208|Acc]);
	to_upper("к"++T, Acc) -> to_upper(T, [154,208|Acc]);
	to_upper("л"++T, Acc) -> to_upper(T, [155,208|Acc]);
	to_upper("м"++T, Acc) -> to_upper(T, [156,208|Acc]);
	to_upper("н"++T, Acc) -> to_upper(T, [157,208|Acc]);
	to_upper("о"++T, Acc) -> to_upper(T, [158,208|Acc]);
	to_upper("п"++T, Acc) -> to_upper(T, [159,208|Acc]);
	to_upper("р"++T, Acc) -> to_upper(T, [160,208|Acc]);
	to_upper("с"++T, Acc) -> to_upper(T, [161,208|Acc]);
	to_upper("т"++T, Acc) -> to_upper(T, [162,208|Acc]);
	to_upper("у"++T, Acc) -> to_upper(T, [163,208|Acc]);
	to_upper("ф"++T, Acc) -> to_upper(T, [164,208|Acc]);
	to_upper("х"++T, Acc) -> to_upper(T, [165,208|Acc]);
	to_upper("ц"++T, Acc) -> to_upper(T, [166,208|Acc]);
	to_upper("ч"++T, Acc) -> to_upper(T, [167,208|Acc]);
	to_upper("ш"++T, Acc) -> to_upper(T, [168,208|Acc]);
	to_upper("щ"++T, Acc) -> to_upper(T, [169,208|Acc]);
	to_upper("ъ"++T, Acc) -> to_upper(T, [170,208|Acc]);
	to_upper("ы"++T, Acc) -> to_upper(T, [171,208|Acc]);
	to_upper("ь"++T, Acc) -> to_upper(T, [172,208|Acc]);
	to_upper("э"++T, Acc) -> to_upper(T, [173,208|Acc]);
	to_upper("ю"++T, Acc) -> to_upper(T, [174,208|Acc]);
	to_upper("я"++T, Acc) -> to_upper(T, [175,208|Acc]);
	% Extra Ukrainian characters
	to_upper("ґ"++T, Acc) -> to_upper(T, [144,210|Acc]);
	to_upper("ї"++T, Acc) -> to_upper(T, [135,208|Acc]);
	to_upper("і"++T, Acc) -> to_upper(T, [143,208|Acc]);
	to_upper("є"++T, Acc) -> to_upper(T, [132,208|Acc]);
	% Polish support
	to_upper("ą"++T, Acc) -> to_upper(T, [132,196|Acc]);
	to_upper("ę"++T, Acc) -> to_upper(T, [152,196|Acc]);
	to_upper("ć"++T, Acc) -> to_upper(T, [134,196|Acc]);
	to_upper("ł"++T, Acc) -> to_upper(T, [129,197|Acc]);
	to_upper("ń"++T, Acc) -> to_upper(T, [131,197|Acc]);
	to_upper("ś"++T, Acc) -> to_upper(T, [154,197|Acc]);
	to_upper("ź"++T, Acc) -> to_upper(T, [185,197|Acc]);
	to_upper("ż"++T, Acc) -> to_upper(T, [187,197|Acc]);
	% Turkish support
	to_upper("ş"++T, Acc) -> to_upper(T, [158,197|Acc]);
	to_upper("ğ"++T, Acc) -> to_upper(T, [158,196|Acc]);
	to_upper("ı"++T, Acc) -> to_upper(T, [176,196|Acc]);

	% Other chars are taken as-is
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
to_name({trans, Tr}) ->
    case proplists:get_value(en, Tr) of
        undefined -> 
            case Tr of
                [{_,V}|_] -> to_name(V);
                _ -> to_name([])
            end;
        V -> to_name(V)
    end;
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
to_name(_, Acc, N) when N >= 80 ->
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
to_name("ß"++T, Acc, I) -> to_name(T, [$s,$s|Acc], I+2);
to_name("ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("Ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("€"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ÿ"++T, Acc, I) -> to_name(T, [$i,$j|Acc], I+2);
to_name("@"++T, Acc, I) -> to_name(T, [$_,$t,$a,$_|Acc], I+4);
% Cyrillic support (from http://en.wikipedia.org/wiki/Romanization_of_Russian)
to_name("А"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("а"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Б"++T, Acc, I) -> to_name(T, [$b|Acc], I+1);
to_name("б"++T, Acc, I) -> to_name(T, [$b|Acc], I+1);
to_name("В"++T, Acc, I) -> to_name(T, [$v|Acc], I+1);
to_name("в"++T, Acc, I) -> to_name(T, [$v|Acc], I+1);
to_name("Г"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("г"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("Д"++T, Acc, I) -> to_name(T, [$d|Acc], I+1);
to_name("д"++T, Acc, I) -> to_name(T, [$d|Acc], I+1);
to_name("Е"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("е"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("Ё"++T, Acc, I) -> to_name(T, [$o,$y|Acc], I+2);
to_name("ё"++T, Acc, I) -> to_name(T, [$o,$y|Acc], I+2);
to_name("Ж"++T, Acc, I) -> to_name(T, [$h,$z|Acc], I+2);
to_name("ж"++T, Acc, I) -> to_name(T, [$h,$z|Acc], I+2);
to_name("З"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
to_name("з"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
to_name("И"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("и"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Й"++T, Acc, I) -> to_name(T, [$j|Acc], I+1);
to_name("й"++T, Acc, I) -> to_name(T, [$j|Acc], I+1);
to_name("К"++T, Acc, I) -> to_name(T, [$k|Acc], I+1);
to_name("к"++T, Acc, I) -> to_name(T, [$k|Acc], I+1);
to_name("Л"++T, Acc, I) -> to_name(T, [$l|Acc], I+1);
to_name("л"++T, Acc, I) -> to_name(T, [$l|Acc], I+1);
to_name("М"++T, Acc, I) -> to_name(T, [$m|Acc], I+1);
to_name("м"++T, Acc, I) -> to_name(T, [$m|Acc], I+1);
to_name("Н"++T, Acc, I) -> to_name(T, [$n|Acc], I+1);
to_name("н"++T, Acc, I) -> to_name(T, [$n|Acc], I+1);
to_name("О"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("о"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("П"++T, Acc, I) -> to_name(T, [$p|Acc], I+1);
to_name("п"++T, Acc, I) -> to_name(T, [$p|Acc], I+1);
to_name("Р"++T, Acc, I) -> to_name(T, [$r|Acc], I+1);
to_name("р"++T, Acc, I) -> to_name(T, [$r|Acc], I+1);
to_name("С"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("с"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("Т"++T, Acc, I) -> to_name(T, [$t|Acc], I+1);
to_name("т"++T, Acc, I) -> to_name(T, [$t|Acc], I+1);
to_name("У"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("у"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ф"++T, Acc, I) -> to_name(T, [$f|Acc], I+1);
to_name("ф"++T, Acc, I) -> to_name(T, [$f|Acc], I+1);
to_name("Х"++T, Acc, I) -> to_name(T, [$h|Acc], I+1);
to_name("х"++T, Acc, I) -> to_name(T, [$h|Acc], I+1);
to_name("Ц"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("ц"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("Ч"++T, Acc, I) -> to_name(T, [$h,$c|Acc], I+2);
to_name("ч"++T, Acc, I) -> to_name(T, [$h,$c|Acc], I+2);
to_name("Ш"++T, Acc, I) -> to_name(T, [$h,$s|Acc], I+2);
to_name("ш"++T, Acc, I) -> to_name(T, [$h,$s|Acc], I+2);
to_name("Щ"++T, Acc, I) -> to_name(T, [$h,$h,$s|Acc], I+3);
to_name("щ"++T, Acc, I) -> to_name(T, [$h,$h,$s|Acc], I+3);
to_name("Ъ"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("ъ"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("Ы"++T, Acc, I) -> to_name(T, [$y|Acc], I+1);
to_name("ы"++T, Acc, I) -> to_name(T, [$y|Acc], I+1);
to_name("Ь"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("ь"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("Э"++T, Acc, I) -> to_name(T, [$h,$e|Acc], I+2);
to_name("э"++T, Acc, I) -> to_name(T, [$h,$e|Acc], I+2);
to_name("Ю"++T, Acc, I) -> to_name(T, [$u,$y|Acc], I+2);
to_name("ю"++T, Acc, I) -> to_name(T, [$u,$y|Acc], I+2);
to_name("Я"++T, Acc, I) -> to_name(T, [$a,$y|Acc], I+2);
to_name("я"++T, Acc, I) -> to_name(T, [$a,$y|Acc], I+2);
% Ukrainian support
to_name("Ґ"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("ґ"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("Ї"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ї"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("І"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("і"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Є"++T, Acc, I) -> to_name(T, [$e,$y|Acc], I+2);
to_name("є"++T, Acc, I) -> to_name(T, [$e,$y|Acc], I+2);
% Polish support
to_name("Ą"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("ą"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Ę"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ę"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("Ć"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("ć"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("Ł"++T, Acc, I) -> to_name(T, [$l|Acc], I+1);
to_name("ł"++T, Acc, I) -> to_name(T, [$l|Acc], I+1);
to_name("Ń"++T, Acc, I) -> to_name(T, [$n|Acc], I+1);
to_name("ń"++T, Acc, I) -> to_name(T, [$n|Acc], I+1);
to_name("Ś"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("ś"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("Ź"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
to_name("ź"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
to_name("Ż"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
to_name("ż"++T, Acc, I) -> to_name(T, [$z|Acc], I+1);
% Turkish support
to_name("Ş"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("ş"++T, Acc, I) -> to_name(T, [$s|Acc], I+1);
to_name("Ğ"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("ğ"++T, Acc, I) -> to_name(T, [$g|Acc], I+1);
to_name("İ"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ı"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
% Some entities - we might want to add generic code here, depends
% on where to_name/1 is used (can we assume that the input is always html?)
to_name("&amp;"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("&lt;"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("&gt;"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
to_name("&#39;"++T, Acc, I) -> to_name(T, [$_|Acc], I+1);
% Other sequences of characters are mapped to $_
to_name([_C|T], [$_|_] = Acc, I) ->
    to_name(T, Acc, I+1);
to_name([_C|T], Acc, I) ->
    to_name(T, [$_|Acc], I+1).


%% @doc Replace a string inside another string
%% Copyright 2008 Rusty Klophaus  (Nitrogen, MIT License)
replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
	Length = length(S1),
	case string:substr(String, 1, Length) of 
		S1 -> 
			S2 ++ replace(string:substr(String, Length + 1), S1, S2);
		_ -> 
			[hd(String)|replace(tl(String), S1, S2)]
	end.

%% @doc Sanitize an utf-8 string, remove all non-utf-8 characters.
sanitize_utf8(L) when is_list(L) -> sanitize_utf8(iolist_to_binary(L));
sanitize_utf8(B) when is_binary(B) -> s_utf8(B, <<>>).
    
    s_utf8(<<>>, Acc) ->
        Acc;
    s_utf8(<<C, Rest/binary>>, Acc) 
        when C < 128 -> 
        s_utf8(Rest, <<Acc/binary, C>>);
    s_utf8(<<X, A, Rest/binary>>, Acc) 
        when X >= 2#11000000, X =< 2#11011111, 
             A >= 2#10000000, A =< 2#10111111 -> 
        s_utf8(Rest, <<Acc/binary, X, A>>);
    s_utf8(<<X, A, B, Rest/binary>>, Acc) 
        when X >= 2#11100000, X =< 2#11101111, 
             A >= 2#10000000, A =< 2#10111111,
             B >= 2#10000000, B =< 2#10111111 -> 
        s_utf8(Rest, <<Acc/binary, X, A, B>>);
    s_utf8(<<X, A, B, C, Rest/binary>>, Acc) 
        when X >= 2#11110000, X =< 2#11110111, 
             A >= 2#10000000, A =< 2#10111111,
             B >= 2#10000000, B =< 2#10111111,
             C >= 2#10000000, C =< 2#10111111 ->
        s_utf8(Rest, <<Acc/binary, X, A, B, C>>);
    s_utf8(<<X, A, B, C, D, Rest/binary>>, Acc) 
        when X >= 2#11111000, X =< 2#11111011, 
             A >= 2#10000000, A =< 2#10111111,
             B >= 2#10000000, B =< 2#10111111,
             C >= 2#10000000, C =< 2#10111111,
             D >= 2#10000000, D =< 2#10111111 ->
        s_utf8(Rest, <<Acc/binary, X, A, B, C, D>>);
    s_utf8(<<X, A, B, C, D, E, Rest/binary>>, Acc) 
        when X >= 2#11111100, X =< 2#11111101, 
             A >= 2#10000000, A =< 2#10111111,
             B >= 2#10000000, B =< 2#10111111,
             C >= 2#10000000, C =< 2#10111111,
             D >= 2#10000000, D =< 2#10111111,
             E >= 2#10000000, E =< 2#10111111 ->
        s_utf8(Rest, <<Acc/binary, X, A, B, C, D, E>>);
    % Drop illegal utf-8 character.
    s_utf8(<<_, Rest/binary>>, Acc) ->
        s_utf8(Rest, Acc).
    

%% @doc Truncate a string.  Append the '...' character at the place of break off.
%% @spec truncate(String, int()) -> String
truncate(undefined, _) ->
	undefined;
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

	%% HTML element (we only allow self closing elements like <br/> and <hr/>)
	truncate([$>|Rest], N, Append, _LastState, Last, in_element, Acc) ->
		truncate(Rest, N, Append, sentence, Last, in_word, [$>|Acc]);

	truncate([C|Rest], N, Append, LastState, Last, in_element, Acc) ->
		truncate(Rest, N, Append, LastState, Last, in_element, [C|Acc]);

	truncate([$<|Rest], N, Append, LastState, _Last, _AccState, Acc) ->
		truncate(Rest, N, Append, LastState, Acc, in_element, [$<|Acc]);

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


truncatewords(undefined, _) ->
	undefined;
truncatewords(S, Words) ->
    truncatewords(S, Words, "…").
truncatewords(S, Words, Append) when is_binary(S) ->
    truncatewords(z_convert:to_list(S), in_space, Words, Append, []);
truncatewords(S, Words, Append) when is_list(S) ->
    truncatewords(S, in_space, Words, Append, []).

    truncatewords(_S, _State, 0, Append, Acc) ->
        lists:reverse(trim_left_func(Acc, fun iswordsep/1), Append);
    truncatewords([], _State, _Words, _Append, Acc) ->
        lists:reverse(Acc);
    truncatewords([C|Rest], in_space, Words, Append, Acc) ->
        case iswordsep(C) of
            true -> truncatewords(Rest, in_space, Words, Append, [C|Acc]);
            false -> truncatewords(Rest, in_word, Words, Append, [C|Acc])
        end;
    truncatewords([C|Rest], in_word, Words, Append, Acc) ->
        case iswordsep(C) of
            true -> truncatewords(Rest, in_space, Words-1, Append, [C|Acc]);
            false -> truncatewords(Rest, in_word, Words, Append, [C|Acc])
        end.

    iswordsep($\s) -> true;
    iswordsep($\n) -> true;
    iswordsep($\r) -> true;
    iswordsep($\t) -> true;
    iswordsep($,) -> true;
    iswordsep($:) -> true;
    iswordsep($;) -> true;
    iswordsep(_) -> false.


%% @doc Split the binary into lines. Line separators can be \r, \n or \r\n.
split_lines(B) when is_binary(B) ->
	split_lines(B, <<>>, []).
	
	split_lines(<<>>, Line, Acc) ->
		lists:reverse([Line|Acc]);
 	split_lines(<<13,10,Rest/binary>>, Line, Acc) ->
		split_lines(Rest, <<>>, [Line|Acc]);
 	split_lines(<<13,Rest/binary>>, Line, Acc) ->
		split_lines(Rest, <<>>, [Line|Acc]);
 	split_lines(<<10,Rest/binary>>, Line, Acc) ->
		split_lines(Rest, <<>>, [Line|Acc]);
	split_lines(<<C, Rest/binary>>, Line, Acc) ->
		split_lines(Rest, <<Line/binary, C>>, Acc).


%% @doc Escape special characters for ical RFC2445 elements
escape_ical(L) when is_list(L) ->
	escape_ical(iolist_to_binary(L));
escape_ical(B) when is_binary(B) ->
	escape_ical(B, <<>>, 0);
escape_ical(A) when is_atom(A) ->
	escape_ical(atom_to_list(A)).

	escape_ical(<<>>, Acc, _N) -> Acc;
	escape_ical(B, Acc, N) when N >= 70 -> escape_ical(B, <<Acc/binary, 13, 10, 32>>, 0);
	escape_ical(<<13, 10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
	escape_ical(<<10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
	escape_ical(<<9, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, 32>>, N+1);
	escape_ical(<<$", Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $">>, N+2);
	escape_ical(<<$,, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $,>>, N+2);
	escape_ical(<<$:, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $", $:, $">>, N+3);
	escape_ical(<<$;, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $;>>, N+2);
	escape_ical(<<$\\, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $\\>>, N+2);
	escape_ical(<<C, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, C>>, N+1).

%% @doc Return true if Start is a prefix of Word
%% @spec starts_with(String, String) -> bool()
starts_with(Start, B) when is_binary(Start), is_binary(B) ->
    StartSize = size(Start),
    case B of
        <<Start:StartSize/binary, _/binary>> -> true;
        _ -> false
    end;
starts_with(Start, String) ->
    starts_with(iolist_to_binary(Start), iolist_to_binary(String)).


%% @doc Return true iff Word ends with End
%% @spec ends_with(String, String) -> bool()
ends_with(End, B) when is_binary(End), is_binary(B) ->
    StartSize = size(B) - size(End),
    case B of
        <<_:StartSize/binary, End/binary>> -> true;
        _ ->false
    end;
ends_with(End, String) ->
    ends_with(iolist_to_binary(End), iolist_to_binary(String)).


%% @doc Return true iff What is found in the string
%% @spec contains(String, String) -> bool()
contains(What, B) when is_binary(What), is_binary(B) ->
    contains(What, size(What), B, 0);
contains(What, String) ->
    contains(iolist_to_binary(What), iolist_to_binary(String)).

    contains(_What, _SizeWhat, B, C) when C > size(B) ->
        false;
    contains(What, SizeWhat, B, C) ->
        case B of
            <<_:C/binary, What:SizeWhat/binary, _/binary>> ->true;
            _ ->contains(What, SizeWhat, B, C + 1)
        end.
%% @doc Split a string, see http://www.erlang.org/pipermail/erlang-questions/2008-October/038896.html
%% @spec split(String, String) -> list()

split(String, []) ->
     split0(String);
split(String, [Sep]) when is_integer(Sep) ->
     split1(String, Sep);
split(String, [C1,C2|L]) when is_integer(C1), is_integer(C2) ->
     split2(String, C1, C2, L).

%% Split a string at "", which is deemed to occur _between_
%% adjacent characters, but queerly, not at the beginning
%% or the end.

split0([C|Cs]) ->
     [[C] | split0(Cs)];
split0([]) ->
     [].

%% Split a string at a single character separator.

split1(String, Sep) ->
     split1_loop(String, Sep, "").

split1_loop([Sep|String], Sep, Rev) ->
     [lists:reverse(Rev) | split1(String, Sep)];
split1_loop([Chr|String], Sep, Rev) ->
     split1_loop(String, Sep, [Chr|Rev]);
split1_loop([], _, Rev) ->
     [lists:reverse(Rev)].

%% Split a string at a multi-character separator
%% [C1,C2|L].  These components are split out for
%% a fast match.

split2(String, C1, C2, L) ->
     split2_loop(String, C1, C2, L, "").

split2_loop([C1|S = [C2|String]], C1, C2, L, Rev) ->
     case split_prefix(L, String)
       of no   -> split2_loop(S, C1, C2, L, [C1|Rev])
        ; Rest -> [lists:reverse(Rev) | split2(Rest, C1, C2, L)]
     end;
split2_loop([Chr|String], C1, C2, L, Rev) ->
     split2_loop(String, C1, C2, L, [Chr|Rev]);
split2_loop([], _, _, _, Rev) ->
     [lists:reverse(Rev)].

split_prefix([C|L], [C|S]) -> split_prefix(L, S);
split_prefix([],    S)     -> S;
split_prefix(_,     _)     -> no.

test() ->
    A = "üçgen",
    A = to_lower(to_upper(A)),
    "ucgen" = to_name(A),

    "a" = first_char("aap"),
    "Ж" = first_char("ЖЖЖxx"),
    "ć" = first_char("ćaap"),
    ok.
