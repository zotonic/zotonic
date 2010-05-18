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

-export([parse_po/1, lc2lang/1, quotes/1,
	 parse_po_bin/1, all_lang/0, 
	 key2str/1, key2str/2, key2str/3, 
	 all_lcs/0, all_lcs/1,
	 reload_custom_lang/1, reload_custom_lang/2,
	 unload_custom_lang/1, unload_custom_lang/2,
	 recreate_db/0, recreate_db/1,
	 store_pofile/2, store_pofile/3, 
	 lang2cset/1, lang2cset/2]).


-define(DEFAULT_SERVER, gettext_server).
-define(GETTEXT_HEADER_INFO, header).

%%% --------------------------------------------------------------------
%%% This is the lookup routine.
%%% --------------------------------------------------------------------
%%% Hopefully, the surrounding code has done its job and
%%% put the language to be used in the process dictionary.
key2str(Key) -> 
    key2str(Key, get(gettext_language)).

key2str(Key, Lang) when is_list(Key) -> 
    key2str(?DEFAULT_SERVER, Key, Lang);
key2str(Server, Key) when is_atom(Server) ->
    key2str(Server, Key, get(gettext_language)).

key2str(Server, Key, Lang) ->
    gen_server:call(Server, {key2str, Key, Lang}, infinity).

%%% --------------------------------------------------------------------

reload_custom_lang(Lang) ->
    reload_custom_lang(?DEFAULT_SERVER, Lang).

reload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {reload_custom_lang, Lang}, infinity).

unload_custom_lang(Lang) ->
    unload_custom_lang(?DEFAULT_SERVER, Lang).

unload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {unload_custom_lang, Lang}, infinity).

all_lcs() ->
    all_lcs(?DEFAULT_SERVER).

all_lcs(Server) ->
    gen_server:call(Server, all_lcs, infinity).


recreate_db() ->
    recreate_db(?DEFAULT_SERVER). 
   
recreate_db(Server) ->
    gen_server:call(Server, recreate_db, infinity).

%%--------------------------------------------------------------------

store_pofile(Lang, File) when is_binary(File) ->
    store_pofile(?DEFAULT_SERVER, Lang, File).

store_pofile(Server, Lang, File) when is_binary(File) ->
    gen_server:call(Server, {store_pofile, Lang, File}, infinity).

%%--------------------------------------------------------------------

lang2cset(Lang) ->
    lang2cset(?DEFAULT_SERVER, Lang).

lang2cset(Server, Lang) ->
    gen_server:call(Server, {lang2cset, Lang}, infinity).


%%% --------------------------------------------------------------------
%%% In case the string is used in a javascript context,
%%% we need to take care of quotes.
%%% --------------------------------------------------------------------

quotes([$'|T]) -> [$\\,$' | quotes(T)];
quotes([$"|T]) -> [$\\,$" | quotes(T)];
quotes([H|T])  -> [H      | quotes(T)];
quotes([])     -> [].


%%% --------------------------------------------------------------------
%%% Parse a PO-file
%%% --------------------------------------------------------------------

parse_po(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    parse_po_bin(Bin).

parse_po_bin(Bin) ->
    parse_po_file(to_list(Bin)).

parse_po_file("msgid" ++ T) ->
    {Key, R0} = get_po_string(T),
    {Val, Rest} = get_msgstr(R0),
    [{Key,Val} | parse_po_file(Rest)];
parse_po_file([_ | T]) ->
    parse_po_file(T);
parse_po_file([]) ->
    [].

get_msgstr("msgstr" ++ T) ->
    get_po_string(T);
get_msgstr([_ | T]) ->
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
get_po_string([$\s|T]) -> get_po_string(T);
get_po_string([$\r|T]) -> get_po_string(T);
get_po_string([$\n|T]) -> get_po_string(T);
get_po_string([$\t|T]) -> get_po_string(T);
get_po_string([$"|T])  -> header_info(eat_string(T)).

%%% only header-info has empty po-string !
header_info({"",R}) -> {?GETTEXT_HEADER_INFO, R};  
header_info(X)      -> X.

eat_string(S) ->
    eat_string(S,[]).

eat_string([$\\,$"|T], Acc)   -> eat_string(T, [$"|Acc]);   % unescape !
eat_string([$\\,$\\ |T], Acc) -> eat_string(T, [$\\|Acc]);  % unescape !
eat_string([$\\,$n |T], Acc)  -> eat_string(T, [$\n|Acc]);  % unescape !
eat_string([$"|T], Acc)       -> eat_more(T,Acc);
eat_string([H|T], Acc)        -> eat_string(T, [H|Acc]).

eat_more([$\s|T], Acc) -> eat_more(T, Acc);
eat_more([$\n|T], Acc) -> eat_more(T, Acc);
eat_more([$\r|T], Acc) -> eat_more(T, Acc);
eat_more([$\t|T], Acc) -> eat_more(T, Acc);
eat_more([$"|T], Acc)  -> eat_string(T, Acc);
eat_more(T, Acc)       -> {lists:reverse(Acc), T}.


to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(L) when is_list(L)    -> L.


%%% --------------------------------------------------------------------
%%% Language Codes
%%% --------------------------------------------------------------------

lc2lang(LC) -> 
    case iso639:lc3lang(LC) of
	""   -> iso639:lc2lang(LC);  % backward compatible
	Lang -> Lang
    end.
    

all_lang() -> iso639:all3lang().
