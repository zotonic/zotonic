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

write_entries(Fd, Labels) ->
    F = fun
            ({Id, Trans, undefined}) ->
                file:write(Fd, "\nmsgid \"\"\n"),
                write_pretty(unicode:characters_to_binary(Id), Fd),
                file:write(Fd, "msgstr \"\"\n"),
                write_pretty(unicode:characters_to_binary(Trans), Fd);
            ({Id, Trans, Finfo}) ->
                io:format(Fd, "~s~n", [fmt_fileinfo(Finfo)]),
        		file:write(Fd, "msgid \"\"\n"),
                write_pretty(unicode:characters_to_binary(Id), Fd),
        		file:write(Fd, "msgstr \"\"\n"),
        		write_pretty(unicode:characters_to_binary(Trans), Fd)
    	end,
    lists:foreach(F, Labels).

-define(ENDCOL, 72).
-define(SEP, <<" ">>).

write_pretty(Binary, Fd) ->
    file:write(Fd, wrap(escape_chars(Binary))).

%% @doc Line-wrap translation strings that exceed the line length
-spec wrap(binary()) -> binary().
wrap(Binary) when byte_size(Binary) =< ?ENDCOL ->
    <<"\"", Binary/binary, "\"\n">>;
wrap(Binary) ->
    <<"\"", (wrap(binary:split(Binary, ?SEP, [global]), ?ENDCOL))/binary>>.

wrap(Parts, Length) ->
    {Line, Acc} = lists:foldl(
        fun
            (Part, {<<>>, Acc}) when byte_size(Part) =< Length ->
                {Part, Acc};
            (Part, {Line, Acc}) when byte_size(Line) + byte_size(Part) =< Length ->
                {<<Line/binary, " ", Part/binary>>, Acc};

            %% start a new line
            (Part, {Line, <<>>}) ->
                {<<Part/binary>>, <<Line/binary>>};
            (Part, {Line, Acc}) ->
                {<<Part/binary>>, <<Acc/binary, " \"\n\"", Line/binary>>}
        end,
        {<<>>, <<>>},
        Parts
    ),
    case Acc of
        <<>> ->
            <<Line/binary, "\"\n">>;
        _ ->
            <<Acc/binary, " \"\n\"", Line/binary, "\"\n">>
    end.

fmt_fileinfo(Finfo) ->
    F = fun({Fname0,LineNo}, Acc) ->
        Fname1 = shorten_path(Fname0),
        iolist_to_binary([["\n#: ", Fname1, ":", z_convert:to_binary(LineNo)], Acc])
	end,
    lists:foldr(F,<<>>,Finfo).

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

shorten_path(Path) ->
    Parts = filename:split(Path),
    Parts1 = [ unicode:characters_to_binary(P) || P <- Parts ],
    Parts2 = case drop_till_module(Parts1) of
        {ok, P1} -> P1;
        error -> Parts1
    end,
    filename:join(Parts2).

drop_till_module([]) -> error;
drop_till_module([ <<"Users">>, _Username, <<"src">> | Rest ]) -> drop_till_module(Rest);
drop_till_module([ <<"home">>, _Username, <<"src">> | Rest ]) -> drop_till_module(Rest);
drop_till_module([ <<"apps">> | _ ] = Path) -> {ok, tl(Path)};
drop_till_module([ <<"apps_user">> | _ ] = Path) -> {ok, tl(Path)};
drop_till_module([ <<"zotonic_mod_", _/binary>> | _ ] = Path) -> {ok, Path};
drop_till_module([ _Mod, <<"priv">> | _ ] = Path) -> {ok, Path};
drop_till_module([ Mod, <<"src">> | Rest ] = Path) ->
    % Prevent paths like "../<username>/src/..." to be matched as a module name.
    case z_utils:ensure_existing_module(Mod) of
        {ok, _} -> {ok, Path};
        {error, _} -> drop_till_module(Rest)
    end;
drop_till_module([ _ | Path ]) -> drop_till_module(Path).

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
    put(fname, <<(z_convert:to_binary(Mod))/binary, ".erl">>),
    ?debug( "++++++ Filename 1 =<~p>~n",[get(fname)]),
    [H | pt(T, Opts, Func)];
%%%
pt([{attribute,_L,yawsfile,Fname} = H | T], Opts, Func) ->
    put(fname, z_convert:to_binary(Fname)),
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

escape_chars(Binary) ->
    escape_chars(Binary, <<>>).

escape_chars(<<"\"", T/binary>>, Acc) ->
    escape_chars(T, <<Acc/binary, "\\\"">>);
escape_chars(<<"\\", T/binary>>, Acc) ->
    escape_chars(T, <<Acc/binary, "\\\\">>);
escape_chars(<<"\n", T/binary>>, Acc) ->
    escape_chars(T, <<Acc/binary, "\\n">>);
escape_chars(<<H, T/binary>>, Acc) ->
    escape_chars(T, <<Acc/binary, H>>);
escape_chars(<<>>, Acc) ->
    Acc.
