%%%-------------------------------------------------------------------
%%% File:      erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template language scanner
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_scanner).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-export([scan/1]). 


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------
scan(Template) ->
    scan(Template, [], {1, 1}, in_text).


identifier_to_keyword({identifier, Pos, String}, {PrevToken, Acc}) when PrevToken == open_tag; PrevToken == all_keyword ->
    %% At the start of a {% .... %} tag we accept all keywords
    RevString = lists:reverse(String),
    Keywords = ["for", "empty", "endfor", "in", "include", "catinclude", "block", "endblock",
        "extends", "overrules", "inherit", "autoescape", "endautoescape", "if", "else", "endif",
        "not", "or", "and", "xor", "comment", "endcomment", "cycle", "firstof",
        "ifchanged", "ifequal", "endifequal", "ifnotequal", "endifnotequal",
        "now", "regroup", "rsc", "spaceless", "endspaceless", "ssi", "templatetag",
        "load", "call", "with", "url", "print", "image", "image_url", "media", "_", "with", "endwith", 
        "all", "lib", "cache", "endcache" ], 
    Type = case lists:member(RevString, Keywords) of
        true -> list_to_atom(RevString ++ "_keyword");
        _ ->    identifier
    end,
    {Type, [{Type, Pos, RevString}|Acc]};
identifier_to_keyword({identifier, Pos, String}, {PrevToken, Acc}) when PrevToken == pipe ->
    %% Accept any filter function, though translate 'not', 'and' and 'or' as they are special keywords
    RevString = lists:reverse(String),
    ReservedErlang = ["not", "and", "or"], 
    NewFilter = case lists:member(RevString, ReservedErlang) of
        true -> "b_" ++ RevString;
        _ ->    RevString
    end,
    {identifier, [{identifier, Pos, NewFilter}|Acc]};
identifier_to_keyword({identifier, Pos, String}, {_PrevToken, Acc}) ->
    %% After the first keyword of a tag we accept a limited set of keywords
    RevString = lists:reverse(String),
    Keywords = ["in", "not", "or", "and", "xor", "firstof", "regroup", "templatetag", "with", "as"], 
    Type = case lists:member(RevString, Keywords) of
        true -> list_to_atom(RevString ++ "_keyword");
        _ ->    identifier
    end,
    {Type, [{Type, Pos, RevString}|Acc]};
identifier_to_keyword({Type, Pos, String}, {_PrevToken, Acc}) ->
    {Type, [{Type, Pos, lists:reverse(String)}|Acc]}.
    

scan([], Scanned, _, in_text) ->
    {_Token, ScannedKeyword} = lists:foldr(fun identifier_to_keyword/2, {'$eof', []}, Scanned),
    {ok, lists:reverse(ScannedKeyword)};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, {in_trans, _}) ->
    {error, "Reached end of file inside a trans block."};

scan([], _Scanned, _, {in_raw, _}) ->
    {error, "Reached end of file inside a raw block."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};

%%we just capture the {% raw %} {% endraw %} tags and pass on string between the tags as is
scan("{%" ++ T, Scanned, {Row, Column}, {in_raw, "{% endraw %}"}) ->
    case find_endraw(T, "%}", Row, Column+2) of
        {ok, T1, Row1, Column1} ->
            scan(T1, Scanned, {Row1, Column1}, in_text);
        notfound ->
            Scanned1 = append_text_char(Scanned, {Row, Column}, ${),
            Scanned2 = append_text_char(Scanned1, {Row, Column}, $%),
            scan(T, Scanned2, {Row, Column + 2}, {in_raw, "{% endraw %}"})
    end;
scan("<!--{%" ++ T, Scanned, {Row, Column}, {in_raw, "<!--{% endraw %}-->"}) ->
    case find_endraw(T, "%}-->", Row, Column+6) of
        {ok, T1, Row1, Column1} ->
            scan(T1, Scanned, {Row1, Column1}, in_text);
        notfound ->
            Scanned1 = append_text_char(Scanned, {Row, Column}, $<),
            Scanned2 = append_text_char(Scanned1, {Row, Column}, $!),
            Scanned3 = append_text_char(Scanned2, {Row, Column}, $-),
            Scanned4 = append_text_char(Scanned3, {Row, Column}, $-),
            Scanned5 = append_text_char(Scanned4, {Row, Column}, ${),
            Scanned6 = append_text_char(Scanned5, {Row, Column}, $%),
            scan(T, Scanned6, {Row, Column + 6}, {in_raw, "<!--{% endraw %}-->"})
    end;

scan("\r\n" ++ T, Scanned, {Row, Column}, {in_raw, Closer}) ->
    Scanned1 = append_text_char(Scanned, {Row, Column}, $\r),
    Scanned2 = append_text_char(Scanned1, {Row, Column}, $\n),
    scan(T, Scanned2, {Row+1, 1}, {in_raw, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, {in_raw, Closer}) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row+1, 1}, {in_raw, Closer});

scan([H | T], Scanned, {Row, Column}, {in_raw, Closer}) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, {in_raw, Closer});

scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, "<!--{{"} | Scanned], {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, "{{"} | Scanned], {Row, Column + 2}, {in_code, "}}"});

scan("<!--{_" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [
			{trans_text, {Row, Column + length("<!--{_")}, ""},
			{open_trans, {Row, Column}, "<!--{_"} | Scanned], 
    {Row, Column + length("<!--{_")}, {in_trans, "_}-->"});

scan("{_" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [
			{trans_text, {Row, Column + 2}, ""},
			{open_trans, {Row, Column}, "{_"} | Scanned], 
		{Row, Column + 2}, {in_trans, "_}"});

scan("_}-->" ++ T, Scanned, {Row, Column}, {in_trans, "_}-->"}) ->
    scan(T, [{close_trans, {Row, Column}, "_}-->"} | Scanned], {Row, Column + length("_}-->")}, in_text);

scan("_}" ++ T, Scanned, {Row, Column}, {in_trans, "_}"}) ->
    scan(T, [{close_trans, {Row, Column}, "_}"} | Scanned], {Row, Column + 2}, in_text);

scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + 2}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, Scanned, {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan([_ | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_comment, Closer});

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, lists:reverse("<!--{%")} | Scanned], 
        {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, lists:reverse("{%")} | Scanned], 
        {Row, Column + 2}, {in_code, "%}"});

scan([H | T], Scanned, {Row, Column}, {in_trans, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_trans, Closer});

scan("\r\n" ++ T, Scanned, {Row, Column}, in_text) ->
    Scanned1 = append_text_char(Scanned, {Row, Column}, $\r),
    Scanned2 = append_text_char(Scanned1, {Row, Column}, $\n),
    scan(T, Scanned2, {Row + 1, 1}, in_text);

scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row + 1, 1}, in_text);

scan("\r\n" ++ T, Scanned, {Row, _Column}, {in_code, Closer}) ->
    scan(T, Scanned, {Row + 1, 1}, {in_code, Closer});

scan("\n" ++ T, Scanned, {Row, _Column}, {in_code, Closer}) ->
    scan(T, Scanned, {Row + 1, 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, in_text);

scan("\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("_\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{trans_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\"" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan("_\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{trans_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, ""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

% end quote
scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

% treat single quotes the same as double quotes
scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

% Closing code blocks
scan("%}-->" ++ T, [{identifier,_,"war"},{open_tag,_,"%{--!<"}|Scanned], {Row, Column}, {_, "%}-->"}) ->
    scan(T, Scanned, {Row, Column + 5}, {in_raw, "<!--{% endraw %}-->"});

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, [{close_tag, {Row, Column}, lists:reverse("%}-->")} | Scanned], {Row, Column + 5}, in_text);

scan("%}" ++ T, [{identifier,_,"war"},{open_tag,_,"%{"}|Scanned], {Row, Column}, {_, "%}"}) ->
    scan(T, Scanned, {Row, Column + 2}, {in_raw, "{% endraw %}"});

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, [{close_tag, {Row, Column}, lists:reverse("%}")} | Scanned], {Row, Column + 2}, in_text);

scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, [{close_var, {Row, Column}, lists:reverse("}}-->")} | Scanned], {Row, Column + 5}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, [{close_var, {Row, Column}, "}}"} | Scanned], {Row, Column + 2}, in_text);

% Expression operators
scan("==" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'==', {Row, Column}, "=="} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("/=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'/=', {Row, Column}, "/="} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("!=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'/=', {Row, Column}, "!="} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan(">=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>=', {Row, Column}, ">="} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("=<" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'=<', {Row, Column}, "=<"} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'=<', {Row, Column}, "<="} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<', {Row, Column}, "<"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(">" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>', {Row, Column}, ">"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("-" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'-', {Row, Column}, "-"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("+" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'+', {Row, Column}, "+"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("*" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'*', {Row, Column}, "*"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("/" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'/', {Row, Column}, "/"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("%" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'%', {Row, Column}, "%"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("("++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'(', {Row, Column}, "("} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(")" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{')', {Row, Column}, ")"} | Scanned], {Row, Column + 1}, {in_code, Closer});

% Other
scan("," ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{comma, {Row, Column}, ","} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("|" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{pipe, {Row, Column}, "|"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{equal, {Row, Column}, "="} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(":" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{colon, {Row, Column}, ":"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("." ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{dot, {Row, Column}, "."} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(" " ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

scan("\t" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

scan("\r\n" ++ T, Scanned, {Row, _Column}, {_, Closer}) ->
    scan(T, Scanned, {Row+1, 1}, {in_code, Closer});

scan("\n" ++ T, Scanned, {Row, _Column}, {_, Closer}) ->
    scan(T, Scanned, {Row+1, 1}, {in_code, Closer});

scan("\r" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});

scan("#" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{hash, {Row, Column}, "#"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("[" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{open_bracket, {Row, Column}, "["} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("]" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{close_bracket, {Row, Column}, "]"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("{" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{open_curly, {Row, Column}, "{"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("}" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{close_curly, {Row, Column}, "}"} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        digit ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, io_lib:format("Illegal character line ~p column ~p", [Row, Column])}
    end;

scan([H | T], Scanned, {Row, Column}, {in_number, Closer}) ->
    case char_type(H) of
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, io_lib:format("Illegal character line ~p column ~p", [Row, Column])}
    end;

scan([H | T], Scanned, {Row, Column}, {in_identifier, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            {error, io_lib:format("Illegal character line ~p column ~p", [Row, Column])}
    end.


% internal functions

append_char(Scanned, Char) ->
    [String | Scanned1] = Scanned,
    [setelement(3, String, [Char | element(3, String)]) | Scanned1].

append_text_char(Scanned, {Row, Column}, Char) ->
    case length(Scanned) of
        0 ->
            [{text, {Row, Column}, [Char]}];
        _ ->
            [Token | Scanned1] = Scanned,
            case element(1, Token) of
                text ->
                    [{text, element(2, Token), [Char | element(3, Token)]} | Scanned1];
                _ ->
                    [{text, element(2, Token), [Char]} | Scanned]
            end
    end.

char_type(Char) ->
    case Char of 
        C when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) or (C == $_) ->
            letter_underscore;
        C when ((C >= $0) and (C =< $9)) ->
            digit;
        _ ->
            undefined
    end.


% Find the 'endraw %}' tag
find_endraw([C|Rest], Closer, Row, Column) when C == 9; C == 32 ->
    find_endraw(Rest, Closer, Row, Column+1);
find_endraw("\r\n" ++ Rest, Closer, Row, _Column) ->
    find_endraw(Rest, Closer, Row+1, 1);
find_endraw("\n" ++ Rest, Closer, Row, _Column) ->
    find_endraw(Rest, Closer, Row+1, 1);
find_endraw("endraw" ++ Rest, Closer, Row, Column) ->
    find_endraw_close(Rest, Closer, Row, Column+6);
find_endraw(_Rest, _Closer, _Row, _Column) ->
    notfound.

find_endraw_close([C|Rest], Closer, Row, Column) when C == 9; C == 32 ->
    find_endraw_close(Rest, Closer, Row, Column+1);
find_endraw_close("\r\n" ++ Rest, Closer, Row, _Column) ->
    find_endraw_close(Rest, Closer, Row+1, 1);
find_endraw_close("\n" ++ Rest, Closer, Row, _Column) ->
    find_endraw_close(Rest, Closer, Row+1, 1);
find_endraw_close("%}-->" ++ Rest, "%}-->", Row, Column) ->
    {ok, Rest, Row, Column+5};
find_endraw_close("%}" ++ Rest, "%}", Row, Column) ->
    {ok, Rest, Row, Column+2};
find_endraw_close(_T, _Closer, _Row, _Colum) ->
    notfound.

