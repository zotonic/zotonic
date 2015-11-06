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

-export([parse_pot/1, parse_po/1, parse_po_bin/1, parse_po_bin/2, test/0]).


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
        {error, _} = Error ->
            lager:error("Error reading po file ~p: ~p", [Fname, Error]),
            []
    end.

parse_po_bin(Bin) ->
    parse_po_bin(Bin, true).

parse_po_bin(Bin, DropEmpty) ->
    lists:reverse(
      lists:foldl(fun ({<<>>, R}, AccIn) ->
                          [{?GETTEXT_HEADER_INFO, R}|AccIn];
                      ({_, <<>>}, AccIn) when DropEmpty ->
                          AccIn;
                      (R, AccIn) ->
                          [R|AccIn]
                  end,
                  [],
                  parse_po_part(Bin))).

parse_po_part(<<"msgid", T/binary>>) ->
    {Key, R0} = get_po_string(T),
    {Val, Rest} = get_msgstr(R0),
    [{Key,Val} | parse_po_part(Rest)];
parse_po_part(<<_/utf8, T/binary>>) ->
    parse_po_part(T);
parse_po_part(<<>>) ->
    [].

get_msgstr(<<"msgstr", T/binary>>) ->
    get_po_string(T);
get_msgstr(<<_/utf8, T/binary>>) ->
    get_msgstr(T).

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


test() ->

    X = parse_po_bin(<<"msgid \"\"
msgstr \"header value\"">>),

[{header, "header value"}] = X,

    X2 = parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"
">>),

[{header, <<"header value">>}, {<<"en">>, <<"nl">>}] = X2,

    X3 = parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"

msgid \"empty trans\"
msgstr \"\"
">>),

[{header, <<"header value">>}, {<<"en">>, <<"nl">>}, {<<"empty trans">>, <<"empty trans">>}] = X3.

