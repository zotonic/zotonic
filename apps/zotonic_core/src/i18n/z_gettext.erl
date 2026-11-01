%% Copyright (c) 2006, Torbjorn Tornkvist, tobbe@tornkvist.org
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of "Torbjorn Tornkvist" nor the names of any other
%%       contributors may be used to endorse or promote products derived from
%%       this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%%%----------------------------------------------------------------------
%%% Created:  27 Oct 2003 by tobbe@bluetail.com
%%% Function: Tools for multi-lingual capabilities,
%%%           similar to GNU gettext.
%%%----------------------------------------------------------------------
%%% Modified: 2010-05-18 by marc@worrell.nl
%%%
%%% Adaptations for Zotonic.
%%% Original code is at http://github.com/noss/erlang-gettext
%%%----------------------------------------------------------------------
-module(z_gettext).

-include_lib("kernel/include/logger.hrl").

-export([parse_pot/1, parse_po/1, parse_po_bin/1, parse_po_bin/2]).

-define(GETTEXT_HEADER_INFO, header).

%%% --------------------------------------------------------------------
%%% Parse a PO-file
%%% --------------------------------------------------------------------

parse_pot(Fname) ->
    parse_po_1(Fname, false).

parse_po(Fname) ->
    parse_po_1(Fname, true).

parse_po_1(Fname, DropEmpty) ->
    case file:read_file(Fname) of
        {ok, Bin} ->
            parse_po_bin(Bin, DropEmpty);
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error reading po file">>,
                in => zotonic_core,
                path => Fname,
                result => error,
                reason => Reason
            }),
            []
    end.

%% @doc Parse singular entries as {Msgid, Text} and plural entries as
%% {{plural, Msgid, MsgidPlural}, {Rule, FormsTuple}}. A missing or invalid
%% Plural-Forms header gives an undefined rule; such entries are not loaded.
parse_po_bin(Bin) ->
    parse_po_bin(Bin, true).

parse_po_bin(Bin, DropEmpty) ->
    Entries = parse_po_part(Bin),
    Rule = plural_rule(Entries),
    lists:reverse(
      lists:foldl(fun ({<<>>, R}, AccIn) ->
                          [{?GETTEXT_HEADER_INFO, R}|AccIn];
                      ({Key, Forms}, AccIn) when is_tuple(Forms) ->
                          case DropEmpty andalso lists:all(fun(T) -> T =:= <<>> orelse T =:= undefined end,
                                  tuple_to_list(Forms)) of
                              true -> AccIn;
                              false -> [{Key, {Rule, Forms}}|AccIn]
                          end;
                      ({_, <<>>}, AccIn) when DropEmpty ->
                          AccIn;
                      (R, AccIn) ->
                          [R|AccIn]
                  end,
                  [],
                  Entries)).

%% Keep the parsed rule with each entry: different modules can have different
%% catalogs, and a fallback language must use its own catalog's plural rule.
plural_rule(Entries) ->
    Header = proplists:get_value(<<>>, Entries, <<>>),
    case re:run(Header, <<"(?:^|\\n)Plural-Forms:[ \\t]*([^\\n]*)">>,
            [caseless, {capture, all_but_first, binary}]) of
        {match, [Value]} ->
            case z_gettext_plural:parse(Value) of
                {ok, Rule} -> Rule;
                {error, _} -> undefined
            end;
        nomatch -> undefined
    end.

parse_po_part(<<"msgctxt", T/binary>>) ->
    {MsgCtxt, R0} = get_po_string(T),
    [{Key, Value}|Rest] = parse_po_part(R0),
    [{{context, MsgCtxt, Key}, Value}|Rest];
parse_po_part(<<"msgid", T/binary>>) ->
    {Key, R0} = get_po_string(T),
    case R0 of
        <<"msgid_plural", R1/binary>> ->
            {Plural, R2} = get_po_string(R1),
            {Forms, Rest} = get_plural_msgstr(R2, #{}),
            [{{plural, Key, Plural}, Forms}|parse_po_part(Rest)];
        _ ->
            {Val, Rest} = get_msgstr(R0),
            [{Key,Val} | parse_po_part(Rest)]
    end;
parse_po_part(<<"#", T/binary>>) ->
    parse_po_part(skip_to_eol(T));
parse_po_part(<<$\r,$\n, T/binary>>) ->
    parse_po_part(T);
parse_po_part(<<$\n, T/binary>>) ->
    parse_po_part(T);
parse_po_part(<<$\r, T/binary>>) ->
    parse_po_part(T);
parse_po_part(<<_/utf8, T/binary>>) ->
    % Some unexpected character, skip this line
    parse_po_part(skip_to_eol(T));
parse_po_part(<<>>) ->
    [].

get_msgstr(<<"msgstr", T/binary>>) ->
    get_po_string(T);
get_msgstr(<<_/utf8, T/binary>>) ->
    get_msgstr(T).

get_plural_msgstr(<<"msgstr[", T/binary>>, Acc) ->
    [Index, R0] = binary:split(T, <<"]">>),
    I = binary_to_integer(Index),
    true = I >= 0 andalso I < 100,
    false = maps:is_key(I, Acc),
    {Value, Rest} = get_po_string(R0),
    get_plural_msgstr(Rest, Acc#{I => Value});
get_plural_msgstr(Rest, Acc) when map_size(Acc) > 0 ->
    Last = lists:max(maps:keys(Acc)),
    {list_to_tuple([maps:get(I, Acc, undefined) || I <- lists:seq(0, Last)]), Rest}.

skip_to_eol(<<>>) -> <<>>;
skip_to_eol(<<"\r\n", T/binary>>) -> T;
skip_to_eol(<<"\n", T/binary>>) -> T;
skip_to_eol(<<"\r", T/binary>>) -> T;
skip_to_eol(<<_/utf8, T/binary>>) -> skip_to_eol(T).

%%%
%%% A PO-string has the same syntax as a C character string.
%%% For example:
%%%
%%%   msgstr ""
%%%     "Hello "
%%%
%%%     "\\World\n"
%%%
%%% Is parsed as: "Hello \World\n"
%%%
get_po_string(<<$\s, T/binary>>) -> get_po_string(T);
get_po_string(<<$\r, T/binary>>) -> get_po_string(T);
get_po_string(<<$\n, T/binary>>) -> get_po_string(T);
get_po_string(<<$\t, T/binary>>) -> get_po_string(T);
get_po_string(<<$", T/binary>>)  -> eat_string(T).

eat_string(S) ->
    eat_string(S, <<>>).

eat_string(<<$\\,$", T/binary>>, Acc)   -> eat_string(T, <<Acc/binary, $">>);   % unescape !
eat_string(<<$\\,$\\ , T/binary>>, Acc) -> eat_string(T, <<Acc/binary, $\\>>);  % unescape !
eat_string(<<$\\,$n , T/binary>>, Acc)  -> eat_string(T, <<Acc/binary, $\n>>);  % unescape !
eat_string(<<$", T/binary>>, Acc)       -> eat_more(T, Acc);
eat_string(<<H/utf8, T/binary>>, Acc)   -> eat_string(T, <<Acc/binary, H/utf8>>).

eat_more(<<$\s, T/binary>>, Acc) -> eat_more(T, Acc);
eat_more(<<$\n, T/binary>>, Acc) -> eat_more(T, Acc);
eat_more(<<$\r, T/binary>>, Acc) -> eat_more(T, Acc);
eat_more(<<$\t, T/binary>>, Acc) -> eat_more(T, Acc);
eat_more(<<$", T/binary>>, Acc)  -> eat_string(T, Acc);
eat_more(T, Acc)       -> {Acc, T}.
