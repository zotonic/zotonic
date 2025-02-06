%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2024 Marc Worrell
%% @doc Convert a html text to markdown syntax.
%% This is used when editing TinyMCE texts with the markdown editor.
%% @end

%% Copyright 2011-2024 Marc Worrell
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
-record(ms, {
    li = none,
    indent = [],
    allow_html = true,
    format_tables = true,
    level = -1
}).

-define(MAX_TABLE_CELL_WIDTH, 80).

-compile({no_auto_import,[max/2]}).

convert(Html) ->
    convert(Html, []).

%% @doc Convert a html text to markdown format. Assumes the html has been sanitized and normalized.
convert(Html, Options) when is_binary(Html) ->
    convert1(<<"<sanitize>",Html/binary,"</sanitize>">>, Options);
convert(Html, Options) when is_list(Html) ->
    convert1(unicode:characters_to_binary(["<sanitize>", Html, "</sanitize>"], utf8), Options).

convert1(Html, Options) ->
    case z_html_parse:parse(Html) of
        {ok, {<<"sanitize">>, _, Parsed}} ->
            {Text, M} = to_md(Parsed, #md{}, set_options(Options, #ms{})),
            iolist_to_binary([trimnl(unicode:characters_to_binary(Text, utf8)), expand_anchors(M)]);
        {error, _} ->
            <<>>
    end.

set_options([], S) ->
    S;
set_options([no_html|T], S) ->
    set_options(T, S#ms{ allow_html = false });
set_options([no_tables|T], S) ->
    set_options(T, S#ms{ format_tables = false }).

-spec to_md(Html, M, S) -> {iodata(), M1} when
    Html :: z_html_parse:html_element()
          | [ z_html_parse:html_element() ],
    M :: #md{},
    S :: #ms{},
    M1 :: #md{}.
to_md(B, M, #ms{ level = 0 }) when is_binary(B) ->
    B1 = z_string:trim(binary:replace(B, <<"\n">>, <<" ">>, [ global ])),
    {escape_html_text(B1, <<>>), M};
to_md(B, M, _S) when is_binary(B) ->
    B1 = binary:replace(B, <<"\n">>, <<" ">>, [ global ]),
    {escape_html_text(B1, <<>>), M};
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
            case EncText of
                [Href] ->
                    {<<$<, Href/binary, $>>>, M1};
                _ ->
                    Link = [
                        $[, trl(EncText), $],
                        $(, Href, $)
                    ],
                    {Link, M1}
                    % {M2,RefNr} = add_anchor(Href, M1),
                    % {[ $[, trl(EncText), $],$[,integer_to_list(RefNr),$] ], M2}
            end
    end;

to_md({<<"code">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$`, z_string:trim(EncText), $`], M1};
to_md({<<"pre">>, Args, Enclosed}, M, S) ->
    case drop_ws(Enclosed) of
        [{<<"code">>, _, EnclosedCode}] ->
            Lang = proplists:get_value(<<"lang">>, Args, <<>>),
            EncText = z_html:unescape(iolist_to_binary(flatten_html(EnclosedCode))),
            EncText1 = case z_string:trim_right(EncText) of
                <<"\n", E/binary>> -> E;
                E -> E
            end,
            Quote = case binary:match(EncText, <<"```">>) of
                nomatch -> <<"```">>;
                _ -> <<"````">>
            end,
            {[
                nl(S),
                Quote, " ", Lang, "\n",
                EncText1, "\n",
                nl(S), Quote,
                nl(S),
                nl(S)
            ], M};
        _ ->
            S1 = S#ms{indent=[code|S#ms.indent]},
            {EncText, M1} = to_md(Enclosed, M, S1),
            {[nl(S1), trl(EncText), nl(S)], M1}
    end;
to_md({<<"div">>, Args, Enclosed}, M, S) ->
    % Check for RST generated texts
    % <div class="highlight-django notranslate"><div class="highlight"><pre>
    %    ...
    % </pre></div></div>
    case proplists:get_value(<<"class">>, Args) of
        <<"highlight-", Class/binary>> ->
            [Lang|_] = binary:split(Class, <<" ">>),
            case drop_ws(Enclosed) of
                [{<<"div">>, [{<<"class">>, <<"highlight">>}|_], Enclosed2}|_] ->
                    case drop_ws(Enclosed2) of
                        [{<<"pre">>, _, EnclosedCode}|_] ->
                            EncText = iolist_to_binary(flatten_text(EnclosedCode)),
                            EncText1 = case z_string:trim_right(EncText) of
                                <<"\n", E/binary>> -> E;
                                E -> E
                            end,
                            Quote = case binary:match(EncText, <<"```">>) of
                                nomatch -> <<"```">>;
                                _ -> <<"````">>
                            end,
                            {[
                                nl(S), Quote, " ", Lang,
                                nl(S), EncText1,
                                nl(S), Quote,
                                nl(S)
                            ], M};
                        _ ->
                            to_md(Enclosed2, M, S)
                    end;
                _ ->
                    to_md(Enclosed, M, S)
            end;
        _ ->
            to_md(Enclosed, M, S)
    end;
to_md({<<"blockquote">>, _Args, Enclosed}, M, S) ->
    S1 = S#ms{indent=[quote|S#ms.indent]},
    {EncText, M1} = to_md(Enclosed, M, S1),
    {[nl(S1), EncText, nl(S)], M1};

to_md({<<"table">>, _Args, Enclosed}, M, #ms{ format_tables = true } = S) ->
    THeads = filter_tags(<<"thead">>, Enclosed),
    TBody = filter_tags(<<"tbody">>, Enclosed),
    BodyRows = case TBody of
        [{_, _, TBs}|_] -> filter_tags(<<"tr">>, TBs);
        [] -> filter_tags(<<"tr">>, Enclosed)
    end,
    HeadRows = case THeads of
        [{_, _, THs}|_] -> filter_tags(<<"tr">>, THs);
        [] -> []
    end,
    {HeadRow, TableRows} = if
        HeadRows =:= [] ->
            case BodyRows of
                [B|Bs] -> {B, Bs};
                [] -> {[], []}
            end;
        true ->
            {hd(HeadRows), BodyRows}
    end,
    HeadCells = case HeadRow of
        [] -> [];
        {_, _, HCs} -> filter_tags([ <<"td">>, <<"th">> ], HCs)
    end,

    % Make MD of all header cells
    {HeadMDCells0, M1} = lists:foldl(
        fun({_, _, CellEnclosed}, {HAcc, MAcc}) ->
            {CellHtml, MAcc1} = to_md(CellEnclosed, MAcc, S),
            {[cell(CellHtml) | HAcc], MAcc1}
        end,
        {[], M},
        HeadCells),
    HeadMDCells = lists:reverse(HeadMDCells0),

    % Make MD of all body rows
    {DataMDRows0, M1} = lists:foldl(
        fun({<<"tr">>, _, DataRow}, {RowAcc, M1Acc}) ->
            {RM1Acc1, M1Acc1} = lists:foldl(
                fun({_, _, CellEnclosed}, {H2Acc, M2Acc}) ->
                    {DataCellHtml, M2Acc1} = to_md(CellEnclosed, M2Acc, S),
                    {[cell(DataCellHtml) | H2Acc], M2Acc1}
                end,
                {[], M1Acc},
                filter_tags(<<"td">>, DataRow)),
            {[ lists:reverse(RM1Acc1) | RowAcc ], M1Acc1}
        end,
        {[], M1},
        TableRows),
    DataMDRows = lists:reverse(DataMDRows0),

    % Remove newlines and trim cells MD
    Widths0 = lists:map(
        fun({_, Args, _}) ->
            case proplists:get_value(<<"align">>, Args) of
                <<"left">> -> 4;
                <<"right">> -> 4;
                <<"center">> -> 5;
                _ -> 3
            end
        end,
        lists:reverse(HeadCells)),
    Widths = column_widths([ HeadMDCells | DataMDRows ], Widths0),
    Aligns = lists:map(
        fun({_, Args, _}) ->
            case proplists:get_value(<<"align">>, Args) of
                <<"left">> -> left;
                <<"right">> -> right;
                <<"center">> -> center;
                _ -> none
            end
        end,
        HeadCells),

    HeadMDCells1 = row(column_pad(HeadMDCells, Widths, Aligns)),
    DataMDRows1 = lists:map(
        fun(Row) ->
            row(column_pad(Row, Widths, Aligns))
        end, DataMDRows),

    % Make alignment dashes from the header classes
    Dashes = lists:map(
        fun
            ({left, W}) -> <<":---", (dashes(W - 4))/binary>>;
            ({right, W}) -> << (dashes(W - 4))/binary, "---:">>;
            ({center, W}) -> <<":---", (dashes(W-5))/binary, ":">>;
            ({_, W}) -> <<"---", (dashes(W-3))/binary>>
        end,
        column_zip(Aligns, Widths)),

    {[HeadMDCells1,
      row(Dashes),
      DataMDRows1,
      "\n"], M};

to_md({<<"ul">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S#ms{li=ul}),
    {[nl(S), EncText, nl(S)], M1};
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

to_md({<<"head">>, _Args, _Enclosed}, M, _S) ->
    {[], M};
to_md({<<"script">>, _Args, _Enclosed}, M, _S) ->
    {[], M};

to_md({_, _, Enclosed}, M, S) ->
    to_md(Enclosed, M, S);
to_md(L, M, S) when is_list(L) ->
    S1 = lev(S),
    lists:foldl(fun(Elt,{AT,AM}) ->
                    {AT1, AM1} = to_md(Elt, AM, S1),
                    {AT++[AT1], AM1}
                end, {[], M}, L);
to_md(_Other, M, _S) ->
    {<<>>, M}.


filter_tags(Tag, Elts) when is_binary(Tag) ->
    filter_tags([Tag], Elts);
filter_tags(Tags, Elts) when is_list(Elts) ->
    lists:filter(
        fun
            ({Elt, _, _}) -> lists:member(Elt, Tags);
            (_) -> false
        end,
        Elts).

header(Char, Enclosed, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    case trl(EncText) of
        [] ->
            {[], M1};
        <<>> ->
            {[], M1};
        Trimmed ->
            {[nl(S), nl(S), Trimmed, nl(S), lists:duplicate(max(len(Trimmed), 3), [Char]), nl(S), nl(S)], M1}
    end.

-spec row(Cells) -> binary() when
    Cells :: [ iodata() ].
row(Cells) ->
    iolist_to_binary([
        "| ",
        lists:join(" | ", Cells),
        " |\n"
    ]).

-spec cell(Text) -> binary() when
    Text :: iodata().
cell(Text) ->
    Text1 = binary:replace(iolist_to_binary(Text), [ <<"\n">>, <<"\r">> ], <<" ">>, [ global ]),
    Text2 = z_string:trim(Text1),
    Text3 = binary:replace(Text2, <<"\\">>, <<"\\\\">>, [ global ]),
    binary:replace(Text3, <<"|">>, <<"\\|">>, [ global ]).

column_widths([], Acc) ->
    Acc;
column_widths([ Row | Rows ], Acc) ->
    Acc1 = column_widths_1(Row, Acc, []),
    column_widths(Rows, Acc1).

column_widths_1([], [], Acc) ->
    Acc1 = lists:map(fun(W) -> min(W, ?MAX_TABLE_CELL_WIDTH) end, Acc),
    lists:reverse(Acc1);
column_widths_1([C|Cs], [], Acc) ->
    column_widths_1(Cs, [], [ z_string:len(C) | Acc ]);
column_widths_1([], [W|Ws], Acc) ->
    column_widths_1([], Ws, [ W | Acc ]);
column_widths_1([C|Cs], [W|Ws], Acc) ->
    column_widths_1(Cs, Ws, [ max(z_string:len(C), W) | Acc ]).

column_zip(Cs, As) ->
    column_zip_1(Cs, As, []).

column_zip_1([], [], Acc) ->
    lists:reverse(Acc);
column_zip_1([C|Cs], [], Acc) ->
    column_zip_1(Cs, [], [ {C, 3} | Acc ]);
column_zip_1([], [A|As], Acc) ->
    column_zip_1([], As, [ {[], A} | Acc ]);
column_zip_1([C|Cs], [A|As], Acc) ->
    column_zip_1(Cs, As, [ {C, A} | Acc ]).

-spec column_pad(Cs, Ws, Align) -> [ binary() ] when
    Cs :: [ binary() ],
    Ws :: [ integer() ],
    Align :: [ Direction ],
    Direction :: right | center | left | none.
column_pad(Cs, Ws, Align) ->
    Zipped = column_zip(column_zip(Cs, Ws), Align),
    lists:map(
        fun ({{C, W}, A}) -> pad(C, W, A) end,
        Zipped).

-spec pad(C, W, Direction) -> binary() when
    C :: binary(),
    W :: integer(),
    Direction :: right | center | left | none.
pad(C, W, right) when is_binary(C) ->
    case z_string:len(C) of
        N when N < W -> <<(spaces(W-N))/binary, C/binary>>;
        _ -> C
    end;
pad(C, W, center) when is_binary(C) ->
    case z_string:len(C) of
        N when N < W ->
            N1 = (W - N) div 2,
            N2 = W - N - N1,
            <<(spaces(N1))/binary, C/binary, (spaces(N2))/binary>>;
        _ -> C
    end;
pad(C, W, _) when is_binary(C) ->
    case z_string:len(C) of
        N when N < W -> <<C/binary, (spaces(W-N))/binary>>;
        _ -> C
    end.

spaces(W) ->
    spaces(W, <<>>).

spaces(N, Acc) when N =< 0 -> Acc;
spaces(N, Acc) -> spaces(N-1, <<Acc/binary, " ">>).

dashes(W) ->
    dashes(W, <<>>).

dashes(N, Acc) when N =< 0 -> Acc;
dashes(N, Acc) -> dashes(N-1, <<Acc/binary, "-">>).

lev(#ms{ level = Level } = S) ->
    S#ms{ level = Level + 1}.

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

drop_ws([]) -> [];
drop_ws([B|T]) when is_binary(B) -> drop_ws(T);
drop_ws(L) -> L.

%% @doc Simple recursive length of an iolist
len(EncText) when is_binary(EncText) ->
    z_string:len(EncText);
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
    escape_html_text(trl(T), <<Acc/binary, $\n>>);
escape_html_text(<<C, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, C>>).


trimnl(<<$\n, Rest/binary>>) ->
    trimnl(Rest);
trimnl(B) ->
    B.

trl(B) ->
    z_string:trim_left(B).


% add_anchor(Href, M) ->
%     case indexof(Href, M#md.a, 1) of
%         undefined ->
%             {M#md{a=M#md.a ++ [Href]}, length(M#md.a)+1};
%         N ->
%             {M, N}
%     end.

%     indexof(_A, [], _N) -> undefined;
%     indexof(A, [A|_], N) -> N;
%     indexof(A, [_|R], N) -> indexof(A, R, N+1).


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
    end;
flatten_html(L) when is_list(L) ->
    lists:map(fun flatten_html/1, L).

flatten_text(Text) when is_binary(Text) ->
    Text;
flatten_text({comment, _Text}) ->
    [];
flatten_text({_Tag, _Args, Enclosed}) ->
    flatten_text(Enclosed);
flatten_text(L) when is_list(L) ->
    lists:map(fun flatten_text/1, L).

is_self_closing(<<"img">>) -> true;
is_self_closing(<<"br">>) -> true;
is_self_closing(<<"hr">>) -> true;
is_self_closing(_) -> false.

flatten_args(Args) ->
    [ flatten_arg(Arg) || Arg <- Args ].

flatten_arg({Name, Value}) ->
    [ 32, Name, $=, $", z_html:escape(Value), $" ].
