%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell

%% @doc Convert a html text to markdown syntax.
%%      This is used when editing TinyMCE texts with the markdown editor.

%% Copyright 2011 Marc Worrell
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


-module(z_html2markdown).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    convert/1,
    convert/2
]).

-include("zotonic.hrl").

% Accumulated markdown state (tree walker)
-record(md, {a=[]}).

% Recursive context dependent markdown state (context)
-record(ms, {li=none, indent=[], allow_html=true}).

-compile({no_auto_import,[max/2]}).

convert(Html) ->
    convert(Html, []).

%% @doc Convert a html text to markdown format. Assumes the html has been sanitized and normalized.
convert(Html, Options) when is_binary(Html) ->
    convert1(<<"<sanitize>",Html/binary,"</sanitize>">>, Options);
convert(Html, Options) when is_list(Html) ->
    convert1(iolist_to_binary(["<sanitize>", Html, "</sanitize>"]), Options).

convert1(Html, Options) ->
    Parsed = mochiweb_html:parse(Html),
    {Text, M} = to_md(Parsed, #md{}, set_options(Options, #ms{})),
    list_to_binary([trimnl(iolist_to_binary(Text)), expand_anchors(M)]).

    set_options([], S) ->
        S;
    set_options([no_html|T], S) ->
        set_options(T, S#ms{allow_html=false}).


to_md(B, M, _S) when is_binary(B) ->
    {escape_html_text(B, <<>>), M};
to_md({comment, _Text}, M, _S) ->
    {<<>>, M};

to_md({<<"h1">>, _Args, Enclosed}, M, S) ->
    header($=, Enclosed, M, S);
to_md({<<"h2">>, _Args, Enclosed}, M, S) ->
    header($-, Enclosed, M, S);
to_md({<<"h",N>>, _Args, Enclosed}, M, S) when N >= $1, N =< $6 ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[nl(S), nl(S), lists:duplicate(N-$0, "#"), 32, EncText, nl(S), nl(S)], M1};

to_md({<<"hr">>, [], []}, M, S) ->
    {[nl(S), nl(S), <<"---">>, nl(S), nl(S)], M};

to_md({<<"br">>, [], []}, M, S) ->
    {[32, 32, nl(S)], M};
to_md({<<"em">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$*, trl(EncText), $*], M1};
to_md({<<"i">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$*, trl(EncText), $*], M1};
to_md({<<"strong">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$*, $*, trl(EncText), $*, $*], M1};
to_md({<<"b">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$*, $*, trl(EncText), $*, $*], M1};

to_md({<<"p">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[trl(EncText), nl(S), nl(S)], M1};

to_md({<<"a">>, Args, Enclosed}, M, S) ->
    case proplists:get_value(<<"href">>, Args) of
        undefined ->
            to_md(Enclosed, M, S);
        Href ->
            {EncText, M1} = to_md(Enclosed, M, S),
            {M2,RefNr} = add_anchor(Href, M1),
            {[ $[, trl(EncText), $],$[,integer_to_list(RefNr),$] ], M2}
    end;
    
to_md({<<"code">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$`, z_string:trim(EncText), $`], M1};
to_md({<<"pre">>, _Args, [{<<"code">>, _, Enclosed}]}, M, S) ->
    S1 = S#ms{indent=[code|S#ms.indent]},
    {EncText, M1} = to_md(Enclosed, M, S1),
    {[nl(S1), trl(EncText), nl(S)], M1};
to_md({<<"pre">>, _Args, Enclosed}, M, S) ->
    S1 = S#ms{indent=[code|S#ms.indent]},
    {EncText, M1} = to_md(Enclosed, M, S1),
    {[nl(S1), trl(EncText), nl(S)], M1};

to_md({<<"quote">>, _Args, Enclosed}, M, S) ->
    S1 = S#ms{indent=[quote|S#ms.indent]},
    {EncText, M1} = to_md(Enclosed, M, S1),
    {[nl(S1), trl(EncText), nl(S)], M1};

to_md({<<"ul">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S#ms{li=ul}),
    {[nl(S), trl(EncText), nl(S)], M1};
to_md({<<"ol">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S#ms{li=ol}),
    {[nl(S), trl(EncText), nl(S)], M1};
to_md({<<"li">>, _Args, Enclosed}, M, S) ->
    Bullet = case S#ms.li of
                ol -> "1.  ";
                ul -> "*   "
             end,
    {EncText, M1} = to_md(Enclosed, M, S#ms{li=none, indent=[S#ms.li|S#ms.indent]}),
    {[nl(S), Bullet, 32, trl(EncText)], M1};

to_md({<<"table">>, _Args, _Enclosed} = Html, M, S) when S#ms.allow_html ->
    {flatten_html(Html), M};

to_md({<<"head">>, _Args, _Enclosed}, M, _S) ->
    {[], M};
to_md({<<"script">>, _Args, _Enclosed}, M, _S) ->
    {[], M};

to_md({_, _, Enclosed}, M, S) ->
    to_md(Enclosed, M, S);
to_md(L, M, S) when is_list(L) ->
    lists:foldl(fun(Elt,{AT,AM}) -> 
                    {AT1,AM1} = to_md(Elt, AM, S),
                    {AT++[AT1], AM1}
                end, {[], M}, L).


header(Char, Enclosed, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    Trimmed = trl(EncText),
    case trl(EncText) of
        [] ->
            {[], M1};
        <<>> ->
            {[], M1};
        Trimmed ->
            {[nl(S), nl(S), Trimmed, nl(S), lists:duplicate(max(len(Trimmed), 3), [Char]), nl(S), nl(S)], M1}
    end.


max(A,B) when A > B -> A;
max(_A,B) -> B.


nl(#ms{indent=[]}) ->
    $\n;
nl(#ms{indent=Indent}) ->
    nl1(Indent, []).

    nl1([], Acc) -> 
        [$\n|Acc];
    nl1([ul|Rest], Acc) -> 
        nl1(Rest, ["   "|Acc]);
    nl1([ol|Rest], Acc) -> 
        nl1(Rest, ["    "|Acc]);
    nl1([code|Rest], Acc) -> 
        nl1(Rest, ["    "|Acc]);
    nl1([quote|Rest], Acc) -> 
        nl1(Rest, ["> "|Acc]).


%% @doc Simple recursive length of an iolist
len(EncText) when is_binary(EncText) ->
    size(EncText);
len(N) when is_integer(N) ->
    1;
len([H|L]) ->
    len(H) + len(L);
len([]) ->
    0.



%% @doc Escape pointy brackets, single and double quotes in texts (ampersand is already removed or escaped).
escape_html_text(<<>>, Acc) -> 
    Acc;
escape_html_text(<<${, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\{">>);
escape_html_text(<<$}, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\}">>);
escape_html_text(<<$[, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\[">>);
escape_html_text(<<$], T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\]">>);
escape_html_text(<<$_, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\_">>);
escape_html_text(<<$*, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "\\*">>);
escape_html_text(<<$`, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "``">>);
escape_html_text(<<$<, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&lt;">>);
escape_html_text(<<$>, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&gt;">>);
escape_html_text(<<$", T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&quot;">>);
escape_html_text(<<$', T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&#39;">>);
escape_html_text(<<32, T/binary>>, Acc) ->
    escape_html_text(trl(T), <<Acc/binary, 32>>);
escape_html_text(<<9, T/binary>>, Acc) ->
    escape_html_text(trl(T), <<Acc/binary, 32>>);
escape_html_text(<<$\n, T/binary>>, Acc) ->
    escape_html_text(trl(T), <<Acc/binary, 32>>);
escape_html_text(<<C, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, C>>).

%% @doc Escape pointy brackets (for in comments)
escape_html_comment(<<>>, Acc) -> 
    Acc;
escape_html_comment(<<$<, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, "&lt;">>);
escape_html_comment(<<$>, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, "&gt;">>);
escape_html_comment(<<C, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, C>>).


trimnl(<<$\n, Rest/binary>>) ->
    trimnl(Rest);
trimnl(B) ->
    B.

trl(B) ->
    z_string:trim_left(B).



% @todo: check if the Href is already defined, if so return existing index
add_anchor(Href, M) ->
    case indexof(Href, M#md.a, 1) of
        undefined ->
            {M#md{a=M#md.a ++ [Href]}, length(M#md.a)+1};
        N ->
            {M, N}
    end.
    
    indexof(_A, [], _N) -> undefined;
    indexof(A, [A|_], N) -> N;
    indexof(A, [_|R], N) -> indexof(A, R, N+1).


expand_anchors(#md{a = []}) ->
    [];
expand_anchors(#md{a = As}) ->
    [10 | expand_anchor(As, 1, []) ].
    
    expand_anchor([], _, Acc) ->
        lists:reverse(Acc);
    expand_anchor([A|As], N, Acc) ->
        Link = [ 32, 32, $[, integer_to_list(N), $], $:, 32, A, 10 ],
        expand_anchor(As, N+1, [Link|Acc]).

        

flatten_html(Text) when is_binary(Text) ->
    z_html:escape(Text);
flatten_html({comment, _Text}) ->
    [];
flatten_html({Tag, Args, Enclosed}) ->
    case Enclosed == [] andalso is_self_closing(Tag) of
        true ->
            [ $<, Tag, flatten_args(Args), $/, $> ];
        false ->
            [
                $<, Tag, flatten_args(Args), $>,
                [ flatten_html(Enc) || Enc <- Enclosed ],
                $<, $/, Tag, $>
            ]
    end.
    
    is_self_closing(<<"img">>) -> true;
    is_self_closing(<<"br">>) -> true;
    is_self_closing(<<"hr">>) -> true;
    is_self_closing(_) -> false.

    flatten_args(Args) ->
        [ flatten_arg(Arg) || Arg <- Args ].
    
    flatten_arg({Name, Value}) ->
        [ 32, Name, $=, $", z_html:escape(Value), $" ].
