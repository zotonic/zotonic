%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2025 Marc Worrell
%% @doc Convert a html text to markdown syntax.
%% This is used when editing TinyMCE texts with the markdown editor.
%% @end

%% Copyright 2011-2025 Marc Worrell
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

-include("../../include/zotonic.hrl").

% Accumulated markdown state (tree walker)
-record(md, {
    a = [],         % Link accumulator
    li = 1          % List item number
}).

% Recursive context dependent markdown state (context)
-record(ms, {
    li = none,                  % Type of li - ul or ol
    allow_html = true,
    format_tables = true,       % Format tables with dashes or remove them (useful for emails)
    in_code = false             % Are we in a code block or not, escaping is influenced
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
            Parsed1 = trim(Parsed),
            {Text, M} = to_md(Parsed1, #md{}, set_options(Options, #ms{})),
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


%% @doc Trim texts after block level tags.
trim(Html) ->
    {Html1, _} = trim(Html, true),
    Html1.

trim(L, IsTrim) when is_list(L) ->
    {L1, IsTrim1} = lists:foldl(
        fun(T, {EltsAcc, IsTrimAcc}) ->
            {T1, IstrimAcc1} = trim(T, IsTrimAcc),
            {[ T1 | EltsAcc ], IstrimAcc1}
        end,
        {[], IsTrim},
        L),
    {lists:reverse(L1), IsTrim1};
trim({<<"pre">>, _Params, _Elts} = Pre, IsTrim) ->
    % Do not trim inside highlighted code blocks
    {Pre, IsTrim};
trim({<<"code">>, _Params, _Elts} = Code, IsTrim) ->
    % Do not trim inside highlighted code blocks
    {Code, IsTrim};
trim({<<"div">>, Params, _Elts} = Div, IsTrim) ->
    case proplists:get_value(<<"class">>, Params) of
        <<"highlight-", _/binary>> ->
            % Do not trim inside highlighted code blocks
            {Div, IsTrim};
        _ ->
            trim_tag(Div, IsTrim)
    end;
trim({_Tag, _Params, _Elts} = HtmlElement, IsTrim) ->
    trim_tag(HtmlElement, IsTrim);
trim(Data, true) when is_binary(Data) ->
    Data1 = z_string:trim_left(drop_nl(Data)),
    {Data1, Data1 =:= <<>>};
trim(Data, false) when is_binary(Data) ->
    {drop_nl(Data), false};
trim(_Other, IsTrim) ->
    {<<>>, IsTrim}.

trim_tag({Tag, Params, Elts}, IsTrim) ->
    case is_block(Tag) of
        true ->
            {Elts1, _} = lists:foldl(
                fun(T, {EltsAcc, IsTrimAcc}) ->
                    {T1, IstrimAcc1} = trim(T, IsTrimAcc),
                    {[ T1 | EltsAcc ], IstrimAcc1}
                end,
                {[], true},
                Elts),
            {{Tag, Params, lists:reverse(Elts1)}, true};
        false ->
            {Elts1, IsTrim1} = lists:foldl(
                fun(T, {EltsAcc, IsTrimAcc}) ->
                    {T1, IstrimAcc1} = trim(T, IsTrimAcc),
                    {[ T1 | EltsAcc ], IstrimAcc1}
                end,
                {[], IsTrim},
                Elts),
            {{Tag, Params, lists:reverse(Elts1)}, IsTrim1}
    end.

drop_nl(Data) ->
    binary:replace(Data, [ <<"\n">>, <<"\r">>, <<"\t">> ], <<" ">>, [ global ]).


-spec to_md(Html, M, S) -> {iodata(), M1} when
    Html :: z_html_parse:html_element()
          | [ z_html_parse:html_element() ],
    M :: #md{},
    S :: #ms{},
    M1 :: #md{}.
to_md(B, M, #ms{ in_code = true }) when is_binary(B) ->
    % Escape text for in code blocks.
    B1 = code_block_text(B, <<>>),
    {B1, M};
to_md(B, M, _S) when is_binary(B) ->
    B1 = markdown_text(B, <<>>),
    {B1, M};
to_md({<<"h1">>, _Args, Enclosed}, M, S) ->
    header($=, Enclosed, M, S);
to_md({<<"h2">>, _Args, Enclosed}, M, S) ->
    header($-, Enclosed, M, S);
to_md({<<"h",N>>, _Args, Enclosed}, M, S) when N >= $1, N =< $6 ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {["\n\n", lists:duplicate(N-$0, "#"), 32, EncText, "\n\n"], M1};

to_md({<<"hr">>, [], []}, M, _S) ->
    {<<"\n\n---\n\n">>, M};

to_md({<<"br">>, [], []}, M, _S) ->
    {["  \n"], M};
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
to_md({<<"del">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    {[$~, $~, trl(EncText), $~, $~], M1};

to_md({<<"p">>, _Args, Enclosed}, M, S) ->
    {EncText, M1} = to_md(Enclosed, M, S),
    EncText1 = z_string:trim(iolist_to_binary(EncText)),
    {[EncText1, "\n\n"], M1};

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
    {EncText, M1} = to_md(Enclosed, M, S#ms{ in_code = true }),
    Trimmed = z_string:trim(EncText),
    case binary:match(Trimmed, <<"`">>) of
        nomatch ->
            {[$`, Trimmed, $`], M1};
        _ ->
            {[<<"`` ">>, Trimmed, <<" ``">>], M1}
    end;
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
                "\n",
                Quote, Lang, "\n",
                EncText1, "\n",
                Quote, "\n",
                "\n"
            ], M};
        _ ->
            {EncText, M1} = to_md(Enclosed, M, S),
            EncText1 = iolist_to_binary(z_string:trim(EncText)),
            EncText2 = binary:replace(EncText1, <<"\n">>, <<"\n> ">>, [ global ]),
            {["\n> ", EncText2, "\n"], M1}
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
                                "\n",
                                Quote, Lang, "\n",
                                EncText1, "\n",
                                Quote, "\n",
                                "\n"
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
    {EncText, M1} = to_md(Enclosed, M, S),
    EncText1 = iolist_to_binary(z_string:trim(EncText)),
    EncText2 = binary:replace(EncText1, <<"\n">>, <<"\n> ">>, [ global ]),
    {["\n> ", EncText2, "\n"], M1};

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
    PrevNth = M#md.li,
    {EncText, M1} = to_md(Enclosed, M#md{ li = 1 }, S#ms{ li = ul }),
    EncText1 = z_string:trim(iolist_to_binary(EncText)),
    {[EncText1, "\n\n"], M1#md{ li = PrevNth }};
to_md({<<"ol">>, _Args, Enclosed}, M, S) ->
    PrevNth = M#md.li,
    {EncText, M1} = to_md(Enclosed, M#md{ li = 1 }, S#ms{ li = ol }),
    EncText1 = z_string:trim(iolist_to_binary(EncText)),
    {[EncText1, "\n\n"], M1#md{ li = PrevNth}};
to_md({<<"li">>, _Args, Enclosed}, M, S) ->
    Bullet = case S#ms.li of
                ol -> iolist_to_binary([ integer_to_binary(M#md.li), "." ]);
                ul -> <<"*">>
             end,
    {EncText, M1} = to_md(Enclosed, M, S#ms{ li = none }),
    Spaces = iolist_to_binary([ " " || _N <- lists:seq(1, size(Bullet) + 3 )]),
    EncText1 = binary:replace(
        z_string:trim(iolist_to_binary(EncText)),
        <<"\n">>,
        <<"\n", Spaces/binary>>,
        [ global ]),
    {["\n", Bullet, "   ", trl(EncText1)], M1#md{ li = 1 + M#md.li }};

to_md({<<"head">>, _Args, _Enclosed}, M, _S) ->
    {[], M};
to_md({<<"script">>, _Args, _Enclosed}, M, _S) ->
    {[], M};

to_md({Elt, _, Enclosed}, M, S) ->
    {Data, M1} = to_md(Enclosed, M, S),
    Data1 = case is_block(Elt) of
        true -> [ z_string:trim_right(Data), "\n", "\n" ];
        false -> Data
    end,
    {Data1, M1};
to_md(L, M, S) when is_list(L) ->
    lists:foldl(fun(Elt,{AT,AM}) ->
                    {AT1, AM1} = to_md(Elt, AM, S),
                    {AT++[AT1], AM1}
                end, {[], M}, L);
to_md(_Other, M, _S) ->
    {<<>>, M}.

is_block(Tag) -> not is_inline(Tag).

is_inline(<<"a">>) -> true;
is_inline(<<"span">>) -> true;
is_inline(<<"u">>) -> true;
is_inline(<<"b">>) -> true;
is_inline(<<"i">>) -> true;
is_inline(<<"em">>) -> true;
is_inline(<<"strong">>) -> true;
is_inline(<<"tt">>) -> true;
is_inline(<<"cite">>) -> true;
is_inline(<<"q">>) -> true;
is_inline(<<"mark">>) -> true;
is_inline(<<"ins">>) -> true;
is_inline(<<"del">>) -> true;
is_inline(<<"font">>) -> true;
is_inline(<<"code">>) -> true;
is_inline(_) -> false.

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
            {["\n\n", Trimmed, "\n", lists:duplicate(max(len(Trimmed), 3), [Char]), "\n\n"], M1}
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
    C :: binary() | iolist(),
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
    end;
pad(C, W, Direction) when is_list(C) ->
    B = iolist_to_binary(C),
    pad(B, W, Direction).

spaces(W) ->
    spaces(W, <<>>).

spaces(N, Acc) when N =< 0 -> Acc;
spaces(N, Acc) -> spaces(N-1, <<Acc/binary, " ">>).

dashes(W) ->
    dashes(W, <<>>).

dashes(N, Acc) when N =< 0 -> Acc;
dashes(N, Acc) -> dashes(N-1, <<Acc/binary, "-">>).

max(A,B) when A > B -> A;
max(_A,B) -> B.

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


%% @doc Markdown-escape of unescaped HTML texts.
%% Used for texts inside HTML code block.
code_block_text(<<>>, Acc) ->
    Acc;
code_block_text(<<$`, T/binary>>, <<>>) ->
    code_block_text(T, <<" `">>);
code_block_text(<<$`>>, Acc) ->
    <<Acc/binary,"` ">>;
code_block_text(<<$`, T/binary>>, Acc) ->
    code_block_text(T, <<Acc/binary, "\\`">>);
code_block_text(<<$>, T/binary>>, Acc) ->
    code_block_text(T, <<Acc/binary, "\\>">>);
code_block_text(<<9, T/binary>>, Acc) ->
    code_block_text(T, <<Acc/binary, 32>>);
code_block_text(<<C, T/binary>>, Acc) ->
    code_block_text(T, <<Acc/binary, C>>).


%% @doc Escape a literal text. Used for normal texts.
%% Special care for escaping HTML entities and possible
%% HTML tags.
markdown_text(<<>>, Acc) ->
    Acc;
markdown_text(<<${, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\{">>);
markdown_text(<<$}, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\}">>);
markdown_text(<<$[, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\[">>);
markdown_text(<<$], T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\]">>);
markdown_text(<<$_, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\_">>);
markdown_text(<<$*, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\*">>);
markdown_text(<<$`, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, "\\`">>);
markdown_text(<<$&, T/binary>>, Acc) ->
    case maybe_entity(T) of
        true -> markdown_text(T, <<Acc/binary, "&amp;">>);
        false -> markdown_text(T, <<Acc/binary, "&">>)
    end;
markdown_text(<<$<, T/binary>>, Acc) ->
    case maybe_tag(T) of
        true -> markdown_text(T, <<Acc/binary, "&lt;">>);
        false -> markdown_text(T, <<Acc/binary, "<">>)
    end;
markdown_text(<<32, T/binary>>, Acc) ->
    markdown_text(trl(T), <<Acc/binary, 32>>);
markdown_text(<<9, T/binary>>, Acc) ->
    markdown_text(trl(T), <<Acc/binary, 32>>);
markdown_text(<<$\n, T/binary>>, Acc) ->
    markdown_text(trl(T), <<Acc/binary, $\n>>);
markdown_text(<<C, T/binary>>, Acc) ->
    markdown_text(T, <<Acc/binary, C>>).


trimnl(<<$\n, Rest/binary>>) ->
    trimnl(Rest);
trimnl(B) ->
    B.

trl(B) ->
    z_string:trim_left(B).


% Check after a "&" if it might have been the start of
% a HTML entity. Erring on the safe side.
maybe_entity(<<>>) ->
    % Never know what comes next - err on the safe side.
    true;
maybe_entity(<<"#", _T/binary>>) ->
    true;
maybe_entity(<<C, T/binary>>) when C >= $a, C =< $z ->
    maybe_entity_1(T);
maybe_entity(<<C, T/binary>>) when C >= $A, C =< $Z ->
    maybe_entity_1(T);
maybe_entity(_) ->
    false.

maybe_entity_1(<<";", _/binary>>) ->
    true;
maybe_entity_1(<<C, T/binary>>) when C >= $a, C =< $z ->
    maybe_entity_1(T);
maybe_entity_1(<<C, T/binary>>) when C >= $A, C =< $Z ->
    maybe_entity_1(T);
maybe_entity_1(_) ->
    false.

% Check after a "<" if it might have been the start of
% a HTML entity. Erring on the safe side.
maybe_tag(<<>>) ->
    % Never know what comes next - err on the safe side.
    true;
maybe_tag(<<C, T/binary>>) when C >= $a, C =< $z ->
    binary:match(T, <<">">>) /= nomatch;
maybe_tag(<<C, T/binary>>) when C >= $A, C =< $Z ->
    binary:match(T, <<">">>) /= nomatch;
maybe_tag(_) ->
    false.

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
