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
%%%
%%%   NB: MAKE SURE TO NOT CALL ANY OTHER MODULE IN THE SYSTEM.
%%%       THIS CODE RUNS IN A PRE-BUILD PHASE !!!
%%%
%%% $Id: gettext_compile.erl,v 1.1 2005/07/18 23:22:35 etnt Exp $
%%%----------------------------------------------------------------------
%%% Changed: 2010-05-18 by marc@worrell.nl
%%% Adapted for use with Zotonic.
%%%----------------------------------------------------------------------

-module(z_gettext_compile).
-export([generate/2, parse_transform/2]).

-ifdef(debug).
%%-define(debug(S,A), io:format( S, A)).
-define(debug(S,A), io:format(get(fd), S, A)).
-else.
%%-define(debug(S,A), io:format(get(fd), S, A)).
-define(debug(S,A), true).
-endif.

-define(EPOT_TABLE, gettext_table).

-include_lib("zotonic.hrl").

%% @doc Generate a .po file from the given label/translation pairs
%% The labels are {Label, Translation, Finfo}
%% Author: Marc Worrell
%% Date: 2010-05-19
generate(Filename, Labels) ->
    {ok,Fd} = file:open(Filename, [write]),
    write_header(Fd),
    write_entries(Fd, Labels),
    ok = file:close(Fd).

write_entries(Fd, L) ->
    LibDir = z_utils:lib_dir(),
    F = fun({Id,Trans,Finfo}) ->
            io:format(Fd, "~n#: ~s~n", [fmt_fileinfo(Finfo, LibDir)]),
    		file:write(Fd, "msgid \"\"\n"),
    		write_pretty(Id, Fd),
    		file:write(Fd, "msgstr \"\"\n"),
    		write_pretty(Trans, Fd)
    	end,
    lists:foreach(F, L).

-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

write_pretty([], _) ->
    true;
write_pretty(Str, Fd) when length(Str) =< ?ENDCOL ->
    write_string(Str, Fd);
write_pretty(Str, Fd) ->
    {Line, Rest} = get_line(Str),
    write_string(Line, Fd),
    write_pretty(Rest, Fd).

write_string(Str, Fd) ->
    file:write(Fd, "\""),
    file:write(Fd, escape_chars(Str)),
    file:write(Fd, "\"\n").


%%% Split the string into substrings, 
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
	{true, Racc, Rest} ->
	    {lists:reverse(Racc ++ [H|Acc]), Rest};
	false ->
	    case reverse_tape(Acc, In) of
		{true, Bacc, Rest} ->
		    {lists:reverse(Bacc), Rest};
		{false,Str} ->
		    %%% Ugh...the word is longer than ENDCOL...
		    split_string(Str, ?ENDCOL)
	    end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot ->
    {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot ->
    false;
find_end([H|T], Sep, N, Pivot, Acc) ->
    find_end(T, Sep, N+1, Pivot, [H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc) ->
    {true, Acc, []}.
    
reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) ->
    {true, In, Str};
reverse_tape([H|T], Str, Sep) ->
    reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep) ->
    {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc) ->
    {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N < End ->
    split_string(T, End, N+1, [H|Acc]);
split_string([], _End, _N, Acc) ->
    {lists:reverse(Acc), []}.
    


fmt_fileinfo(Finfo, LibDir) ->
    F = fun({Fname,LineNo}, Acc) ->
        Fname1 = case lists:prefix(LibDir, Fname) of
                    true -> [$.|lists:nthtail(length(LibDir), Fname)];
                    false -> Fname
                 end,
		Fname1 ++ ":" ++ to_list(LineNo) ++ [$\s|Acc]
	end,
    lists:foldr(F,[],Finfo).
		

   
write_header(Fd) ->
    file:write(Fd,
	      "# SOME DESCRIPTIVE TITLE.\n"
	      "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n"
	      "# This file is distributed under the same license as the PACKAGE package.\n"
	      "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n"
	      "#\n"
	      "# NB: Consider using poEdit <http://poedit.sourceforge.net>\n"
	      "#\n"
	      "#\n"
	      "#, fuzzy\n"
	      "msgid \"\"\n"
	      "msgstr \"\"\n"
	      "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
	      "\"POT-Creation-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
	      "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
	      "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
	      "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
	      "\"MIME-Version: 1.0\\n\"\n"
	      "\"Content-Type: text/plain; charset=utf-8\\n\"\n"
	      "\"Content-Transfer-Encoding: 8bit\\n\"\n").


%print_date() ->
%    % 2003-10-21 16:45+0200
%    {{Y,M,D},{H,I,_S}} = erlang:universaltime(),
%    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B+0000", [Y,M,D,H,I])).
    
 
%%% --------------------------------------------------------------------
%%% NB: We assume that the surrounding code does some preparations:
%%%
%%%   1. Setup the environment variables: 'gettext_dir' and 'gettext_tmp_name'
%%%   
%%%   2. The compiler is called with the 'gettext' flag.
%%%
%%%   3. The file $(gettext_dir)/lang/$(gettext_tmp_name)/epot.dets is 
%%%      removed before the first erlang/yaws file is processed.
%%%      (entrys are appended to the file)
%%% --------------------------------------------------------------------
parse_transform(Form,Opts) ->
    case lists:member(gettext, Opts) of
	true ->
	    {_Gettext_App_Name, _GtxtDir, _} = get_env(),
	    %open_epot_file(Gettext_App_Name, GtxtDir),
	    ?debug( "--- Opts --- ~p~n",[Opts]),
	    ?debug("--- Env --- isd_type=~p , gettext_dir=~p~n", [Gettext_App_Name,GtxtDir]),
	    pt(Form, Opts),
	    %close_file(),
	    Form;
	_ ->
	    Form
    end.

get_env() ->
    {os:getenv("gettext_tmp_name"),
     os:getenv("gettext_dir"),
     os:getenv("gettext_def_lang")}.


pt(Form, Opts) ->
    put(fname, ""),
    pt(Form, Opts, undefined).

pt([H|T],Opts,Func) when is_list(H) ->
    ?debug( "--- 1 --- ~p~n",[H]),
    F = fun (X) -> pt(X,Opts,Func) end,
    [lists:map(F,H)|pt(T,Opts,Func)];
%%%
pt({call,L1,{remote,L2,{atom,L3,gettext},{atom,L4,key2str}},
    [{string,L5,String}]}, _Opts, _Func) ->
    ?debug( "++++++ String=<~p>~n",[String]),
    dump(String, L5),
    {call,L1,
     {remote,L2,
      {atom,L3,gettext},
      {atom,L4,key2str}},
     [{string,L5,String}]};
%%%
pt([{call,_,{remote,_,{atom,_,gettext},{atom,_,key2str}},
    [{string,L5,String}]} = H | T], Opts, Func) ->
    ?debug( "++++++ String=<~p>~n",[String]),
    dump(String, L5),
    [H | pt(T, Opts, Func)];
%%%
pt([{attribute,_L,module,Mod} = H | T], Opts, Func) ->
    put(fname, to_list(Mod) ++ ".erl"),
    ?debug( "++++++ Filename 1 =<~p>~n",[get(fname)]),
    [H | pt(T, Opts, Func)];
%%%
pt([{attribute,_L,yawsfile,Fname} = H | T], Opts, Func) ->
    put(fname, to_list(Fname)),
    ?debug( "++++++ Filename 2 =<~p>~n",[get(fname)]),
    [H | pt(T, Opts, Func)];
%%%
pt([{block,N,B}|T], Opts, Func) ->
    ?debug( "--- 2 --- ~p~n",[block]),
    Block = {block,N,pt(B,Opts,Func)},
    [Block|pt(T, Opts, Func)];
%%%
pt([H|T], Opts, Func) when is_tuple(H) ->
    ?debug( "--- 3 --- ~p~n",[H]),
    [while(size(H), H, Opts, Func) | pt(T, Opts, Func)];
%%%
pt([H|T], Opts, Func) ->
    ?debug( "--- 4 --- ~p~n",[H]),
    [H | pt(T, Opts, Func)];
%%%
pt(T, Opts, Func) when is_tuple(T) ->
    ?debug( "--- 5 --- ~p~n",[T]),
    while(size(T), T, Opts, Func);
%%%
pt(X, _, _) ->
    ?debug( "--- 6 --- ~p~n",[X]),
    X.

while(_,{block,N,B},Opts,Func) ->
    {block,N,pt(B,Opts,Func)};
while(N,T,Opts,Func) when N>0 ->
    NT = setelement(N,T,pt(element(N,T),Opts,Func)),
    while(N-1,NT,Opts,Func);
while(0,T,_,_) ->
    T.


dump(Str,L) -> 
    Fname = get(fname),
    Finfo = get_file_info(Str),
    dets:insert(?EPOT_TABLE, {escape_chars(Str), [{Fname,L}|Finfo]}).

get_file_info(Key) ->
    case dets:lookup(?EPOT_TABLE, Key) of
	[]            -> [];
	[{_,Finfo}|_] -> Finfo
    end.

escape_chars(Str) ->
    F = fun($", Acc)  -> [$\\,$"|Acc];
           ($\\, Acc) -> [$\\,$\\|Acc];
           ($\n, Acc) -> [$\\,$n|Acc];
	   (C, Acc)   -> [C|Acc] 
	end,
    lists:foldr(F, [], Str).


to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(L) when is_list(L)    -> L.
