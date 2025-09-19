%% @copyright 2025 Marc Worrell
%% @doc Scanner for the SPARQL 1.1 query grammar in z_sparql_parser.yrl
%% The scanner is based on '19 SPARQL Grammar'. All tokens have a source file, row,
%% and column for better error reporting.
%%
%% See: https://www.w3.org/TR/sparql11-query/#grammar
%% @end

-module(z_sparql_scanner).

-export([scan/1, scan/2]).

-export_type([query/0, position/0, token/0, tokens/0]).

-type query() :: unicode:chardata().
-type source_ref() :: term().
-type position() :: {source_ref(), pos_integer(), pos_integer()}.
-type token() :: {atom(), position(), binary()}.
-type tokens() :: [token()].

%%====================================================================
%% API
%%====================================================================

-spec scan(Query) -> Result
    when
        Query :: query(),
        Result :: {ok, tokens()} | {error, binary()}.
scan(Query) ->
    scan(undefined, Query).

-spec scan(SourceRef, Query) -> Result
    when
        SourceRef :: source_ref(),
        Query :: query(),
        Result :: {ok, tokens()} | {error, binary()}.
scan(SourceRef, Query) when is_list(Query) ->
    scan(SourceRef, unicode:characters_to_binary(Query));
scan(SourceRef, Query) when is_binary(Query) ->
    case decode_codepoints(Query) of
        {ok, Decoded} ->
            scan(Decoded, [], {SourceRef, 1, 1});
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Scanner
%%====================================================================

scan(<<>>, Scanned, _Pos) ->
    {ok, lists:reverse(Scanned)};

scan(<<$\s, Rest/binary>>, Scanned, Pos) ->
    scan(Rest, Scanned, advance($\s, Pos));
scan(<<$\t, Rest/binary>>, Scanned, Pos) ->
    scan(Rest, Scanned, advance($\t, Pos));
scan(<<"\r\n", Rest/binary>>, Scanned, Pos) ->
    scan(Rest, Scanned, newline(Pos));
scan(<<$\r, Rest/binary>>, Scanned, Pos) ->
    scan(Rest, Scanned, newline(Pos));
scan(<<$\n, Rest/binary>>, Scanned, Pos) ->
    scan(Rest, Scanned, newline(Pos));

scan(<<$#, Rest/binary>>, Scanned, Pos) ->
    {Rest1, Pos1} = skip_comment(Rest, advance($#, Pos)),
    scan(Rest1, Scanned, Pos1);

scan(<<"'''", Rest/binary>>, Scanned, Pos) ->
    scan_string(Rest, Scanned, Pos, string_literal_long1, $', long, <<"'''">>);
scan(<<"\"\"\"", Rest/binary>>, Scanned, Pos) ->
    scan_string(Rest, Scanned, Pos, string_literal_long2, $", long, <<"\"\"\"">>);
scan(<<$', Rest/binary>>, Scanned, Pos) ->
    scan_string(Rest, Scanned, Pos, string_literal1, $', short, <<"'">>);
scan(<<$", Rest/binary>>, Scanned, Pos) ->
    scan_string(Rest, Scanned, Pos, string_literal2, $", short, <<"\"">>);

scan(<<$<, _/binary>> = Query, Scanned, Pos) ->
    case take_iri(Query, Pos) of
        {ok, Iri, Rest, Pos1} ->
            scan(Rest, [token(iri_ref, Pos, Iri) | Scanned], Pos1);
        not_iri ->
            scan_operator(Query, Scanned, Pos)
    end;

scan(<<$?, Rest/binary>>, Scanned, Pos) ->
    scan_variable(Rest, Scanned, Pos, var1, $?);
scan(<<$$, Rest/binary>>, Scanned, Pos) ->
    scan_variable(Rest, Scanned, Pos, var2, $$);

scan(<<"_:", Rest/binary>>, Scanned, Pos) ->
    scan_blank_node(Rest, Scanned, Pos);

scan(<<$@, Rest/binary>>, Scanned, Pos) ->
    scan_langtag(Rest, Scanned, Pos);

scan(<<$(, _/binary>> = Query, Scanned, Pos) ->
    case take_empty(Query, $(, $), Pos) of
        {ok, Lexeme, Rest, Pos1} ->
            scan(Rest, [token(nil, Pos, Lexeme) | Scanned], Pos1);
        no ->
            scan_operator(Query, Scanned, Pos)
    end;
scan(<<$[, _/binary>> = Query, Scanned, Pos) ->
    case take_empty(Query, $[, $], Pos) of
        {ok, Lexeme, Rest, Pos1} ->
            scan(Rest, [token(anon, Pos, Lexeme) | Scanned], Pos1);
        no ->
            scan_operator(Query, Scanned, Pos)
    end;

scan(<<Sign, Next, _/binary>> = Query, Scanned, Pos)
  when (Sign =:= $+ orelse Sign =:= $-),
       (Next >= $0 andalso Next =< $9) ->
    scan_number(Query, Scanned, Pos, Sign);
scan(<<Sign, $., Next, _/binary>> = Query, Scanned, Pos)
  when (Sign =:= $+ orelse Sign =:= $-),
       (Next >= $0 andalso Next =< $9) ->
    scan_number(Query, Scanned, Pos, Sign);
scan(<<C, _/binary>> = Query, Scanned, Pos) when C >= $0, C =< $9 ->
    scan_number(Query, Scanned, Pos, unsigned);
scan(<<$., C, _/binary>> = Query, Scanned, Pos) when C >= $0, C =< $9 ->
    scan_number(Query, Scanned, Pos, unsigned);

scan(<<$:, Rest/binary>>, Scanned, Pos) ->
    scan_prefixed_name(<<>>, Rest, Scanned, Pos, advance($:, Pos));

scan(<<C/utf8, _/binary>> = Query, Scanned, Pos) ->
    case is_pn_chars_base(C) of
        true ->
            scan_word_or_prefixed_name(Query, Scanned, Pos);
        false ->
            scan_operator(Query, Scanned, Pos)
    end;
scan(Invalid, _Scanned, Pos) ->
    {error, format_error(Pos, "Invalid UTF-8 input near ~tp", [Invalid])}.

scan_operator(<<"^^", Rest/binary>>, Scanned, Pos) ->
    add_token(hat2, <<"^^">>, Rest, Scanned, Pos);
scan_operator(<<"&&", Rest/binary>>, Scanned, Pos) ->
    add_token(andand, <<"&&">>, Rest, Scanned, Pos);
scan_operator(<<"||", Rest/binary>>, Scanned, Pos) ->
    add_token(oror, <<"||">>, Rest, Scanned, Pos);
scan_operator(<<"!=", Rest/binary>>, Scanned, Pos) ->
    add_token(ne, <<"!=">>, Rest, Scanned, Pos);
scan_operator(<<"<=", Rest/binary>>, Scanned, Pos) ->
    add_token(le, <<"<=">>, Rest, Scanned, Pos);
scan_operator(<<">=", Rest/binary>>, Scanned, Pos) ->
    add_token(ge, <<">=">>, Rest, Scanned, Pos);
scan_operator(<<${, Rest/binary>>, Scanned, Pos) ->
    add_token(lbrace, <<"{">>, Rest, Scanned, Pos);
scan_operator(<<$}, Rest/binary>>, Scanned, Pos) ->
    add_token(rbrace, <<"}">>, Rest, Scanned, Pos);
scan_operator(<<$(, Rest/binary>>, Scanned, Pos) ->
    add_token(lparen, <<"(">>, Rest, Scanned, Pos);
scan_operator(<<$), Rest/binary>>, Scanned, Pos) ->
    add_token(rparen, <<")">>, Rest, Scanned, Pos);
scan_operator(<<$[, Rest/binary>>, Scanned, Pos) ->
    add_token(lbracket, <<"[">>, Rest, Scanned, Pos);
scan_operator(<<$], Rest/binary>>, Scanned, Pos) ->
    add_token(rbracket, <<"]">>, Rest, Scanned, Pos);
scan_operator(<<$., Rest/binary>>, Scanned, Pos) ->
    add_token(dot, <<".">>, Rest, Scanned, Pos);
scan_operator(<<$,, Rest/binary>>, Scanned, Pos) ->
    add_token(comma, <<",">>, Rest, Scanned, Pos);
scan_operator(<<$;, Rest/binary>>, Scanned, Pos) ->
    add_token(semicolon, <<";">>, Rest, Scanned, Pos);
scan_operator(<<$+, Rest/binary>>, Scanned, Pos) ->
    add_token(plus, <<"+">>, Rest, Scanned, Pos);
scan_operator(<<$-, Rest/binary>>, Scanned, Pos) ->
    add_token(minus, <<"-">>, Rest, Scanned, Pos);
scan_operator(<<$*, Rest/binary>>, Scanned, Pos) ->
    add_token(star, <<"*">>, Rest, Scanned, Pos);
scan_operator(<<$/, Rest/binary>>, Scanned, Pos) ->
    add_token(slash, <<"/">>, Rest, Scanned, Pos);
scan_operator(<<$!, Rest/binary>>, Scanned, Pos) ->
    add_token(bang, <<"!">>, Rest, Scanned, Pos);
scan_operator(<<$=, Rest/binary>>, Scanned, Pos) ->
    add_token(eq, <<"=">>, Rest, Scanned, Pos);
scan_operator(<<$<, Rest/binary>>, Scanned, Pos) ->
    add_token(lt, <<"<">>, Rest, Scanned, Pos);
scan_operator(<<$>, Rest/binary>>, Scanned, Pos) ->
    add_token(gt, <<">">>, Rest, Scanned, Pos);
scan_operator(<<$^, Rest/binary>>, Scanned, Pos) ->
    add_token(hat, <<"^">>, Rest, Scanned, Pos);
scan_operator(<<$|, Rest/binary>>, Scanned, Pos) ->
    add_token(pipe, <<"|">>, Rest, Scanned, Pos);
scan_operator(<<$?, Rest/binary>>, Scanned, Pos) ->
    add_token(question, <<"?">>, Rest, Scanned, Pos);
scan_operator(<<C/utf8, _/binary>>, _Scanned, Pos) ->
    {error, format_error(Pos, "Illegal character ~tp", [<<C/utf8>>])};
scan_operator(Invalid, _Scanned, Pos) ->
    {error, format_error(Pos, "Invalid UTF-8 input near ~tp", [Invalid])}.

add_token(Type, Lexeme, Rest, Scanned, Pos) ->
    scan(Rest, [token(Type, Pos, Lexeme) | Scanned], advance_binary(Lexeme, Pos)).

%%====================================================================
%% Terminal scanners
%%====================================================================

scan_string(Rest, Scanned, Pos, Type, Quote, Length, Opener) ->
    Pos1 = advance_binary(Opener, Pos),
    case take_string(Rest, Quote, Length, Pos1, []) of
        {ok, Value, Rest1, Pos2} ->
            scan(Rest1, [token(Type, Pos, Value) | Scanned], Pos2);
        {error, Reason} ->
            {error, format_error(Pos, "~ts", [Reason])}
    end.

take_string(<<Quote, Quote, Quote, Rest/binary>>, Quote, long, Pos, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), Rest,
        advance_binary(<<Quote, Quote, Quote>>, Pos)};
take_string(<<Quote, Rest/binary>>, Quote, short, Pos, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), Rest, advance(Quote, Pos)};
take_string(<<$\\, Escape, Rest/binary>>, Quote, Length, Pos, Acc) ->
    case escaped_char(Escape) of
        {ok, C} ->
            take_string(Rest, Quote, Length, advance(Escape, advance($\\, Pos)),
                [<<C/utf8>> | Acc]);
        error ->
            {error, iolist_to_binary(io_lib:format("Invalid string escape \\\\~c", [Escape]))}
    end;
take_string(<<$\\>>, _Quote, _Length, _Pos, _Acc) ->
    {error, <<"Unterminated string escape">>};
take_string(<<"\r\n", _/binary>>, _Quote, short, _Pos, _Acc) ->
    {error, <<"Newline in short string literal">>};
take_string(<<"\n", _/binary>>, _Quote, short, _Pos, _Acc) ->
    {error, <<"Newline in short string literal">>};
take_string(<<"\r", _/binary>>, _Quote, short, _Pos, _Acc) ->
    {error, <<"Newline in short string literal">>};
take_string(<<"\r\n", Rest/binary>>, Quote, long, Pos, Acc) ->
    take_string(Rest, Quote, long, newline(Pos), [<<"\r\n">> | Acc]);
take_string(<<"\n", Rest/binary>>, Quote, long, Pos, Acc) ->
    take_string(Rest, Quote, long, newline(Pos), [<<"\n">> | Acc]);
take_string(<<"\r", Rest/binary>>, Quote, long, Pos, Acc) ->
    take_string(Rest, Quote, long, newline(Pos), [<<"\r">> | Acc]);
take_string(<<C/utf8, Rest/binary>>, Quote, Length, Pos, Acc) ->
    take_string(Rest, Quote, Length, advance(C, Pos), [<<C/utf8>> | Acc]);
take_string(<<>>, _Quote, _Length, _Pos, _Acc) ->
    {error, <<"Unterminated string literal">>}.

escaped_char($t) -> {ok, $\t};
escaped_char($b) -> {ok, $\b};
escaped_char($n) -> {ok, $\n};
escaped_char($r) -> {ok, $\r};
escaped_char($f) -> {ok, $\f};
escaped_char($\\) -> {ok, $\\};
escaped_char($") -> {ok, $"};
escaped_char($') -> {ok, $'};
escaped_char(_) -> error.

take_iri(<<$<, Rest/binary>>, Pos) ->
    take_iri(Rest, advance($<, Pos), Pos, []).

take_iri(<<$>, Rest/binary>>, Pos, _StartPos, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), Rest, advance($>, Pos)};
take_iri(<<C/utf8, Rest/binary>>, Pos, StartPos, Acc) ->
    case is_iri_char(C) of
        true ->
            take_iri(Rest, advance(C, Pos), StartPos, [<<C/utf8>> | Acc]);
        false ->
            not_iri
    end;
take_iri(<<>>, _Pos, _StartPos, _Acc) ->
    not_iri.

is_iri_char(C) when C =< 16#20 -> false;
is_iri_char($<) -> false;
is_iri_char($") -> false;
is_iri_char(${) -> false;
is_iri_char($}) -> false;
is_iri_char($|) -> false;
is_iri_char($^) -> false;
is_iri_char($`) -> false;
is_iri_char($\\) -> false;
is_iri_char(_) -> true.

scan_variable(Rest, Scanned, Pos, Type, Sigil) ->
    Pos1 = advance(Sigil, Pos),
    case take_varname(Rest, Pos1) of
        {ok, Name, Rest1, Pos2} ->
            scan(Rest1, [token(Type, Pos, Name) | Scanned], Pos2);
        no when Sigil =:= $? ->
            add_token(question, <<"?">>, Rest, Scanned, Pos);
        no ->
            {error, format_error(Pos, "Expected a variable name after ~c", [Sigil])}
    end.

take_varname(<<C/utf8, Rest/binary>>, Pos) ->
    case is_varname_start(C) of
        true -> take_varname_rest(Rest, advance(C, Pos), [<<C/utf8>>]);
        false -> no
    end;
take_varname(<<>>, _Pos) ->
    no.

take_varname_rest(<<C/utf8, Rest/binary>>, Pos, Acc) ->
    case is_varname_char(C) of
        true -> take_varname_rest(Rest, advance(C, Pos), [<<C/utf8>> | Acc]);
        false -> {ok, iolist_to_binary(lists:reverse(Acc)), <<C/utf8, Rest/binary>>, Pos}
    end;
take_varname_rest(<<>>, Pos, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), <<>>, Pos}.

scan_blank_node(Rest, Scanned, Pos) ->
    Pos1 = advance_binary(<<"_:" >>, Pos),
    case take_blank_node_label(Rest, Pos1) of
        {ok, Label, Rest1, Pos2} ->
            scan(Rest1, [token(blank_node_label, Pos, Label) | Scanned], Pos2);
        no ->
            {error, format_error(Pos, "Invalid blank node label", [])}
    end.

take_blank_node_label(<<C/utf8, Rest/binary>>, Pos) ->
    case is_pn_chars_u(C) orelse is_digit(C) of
        true ->
            Pos1 = advance(C, Pos),
            Acc = [<<C/utf8>>],
            take_blank_node_label_rest(Rest, Pos1, Acc, {Rest, Pos1, Acc});
        false ->
            no
    end;
take_blank_node_label(<<>>, _Pos) ->
    no.

take_blank_node_label_rest(<<C/utf8, Rest/binary>>, Pos, Acc, Checkpoint) ->
    case {is_pn_chars(C), C =:= $.} of
        {true, _} ->
            Pos1 = advance(C, Pos),
            Acc1 = [<<C/utf8>> | Acc],
            take_blank_node_label_rest(Rest, Pos1, Acc1, {Rest, Pos1, Acc1});
        {false, true} ->
            take_blank_node_label_rest(Rest, advance(C, Pos), [<<C>> | Acc], Checkpoint);
        _ ->
            blank_node_checkpoint(Checkpoint)
    end;
take_blank_node_label_rest(<<>>, _Pos, _Acc, Checkpoint) ->
    blank_node_checkpoint(Checkpoint).

blank_node_checkpoint({Rest, Pos, Acc}) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), Rest, Pos}.

scan_langtag(Rest, Scanned, Pos) ->
    Pos1 = advance($@, Pos),
    case take_ascii_letters(Rest, Pos1, []) of
        {<<>>, _Rest1, _Pos2} ->
            {error, format_error(Pos, "Invalid language tag", [])};
        {Language, Rest1, Pos2} ->
            case take_langtag_parts(Rest1, Pos2, [Language]) of
                {ok, Parts, Rest2, Pos3} ->
                    Value = iolist_to_binary(lists:join(<<"-">>, Parts)),
                    scan(Rest2, [token(langtag, Pos, Value) | Scanned], Pos3);
                error ->
                    {error, format_error(Pos, "Invalid language tag", [])}
            end
    end.

take_langtag_parts(<<$-, Rest/binary>>, Pos, Acc) ->
    Pos1 = advance($-, Pos),
    case take_ascii_alnum(Rest, Pos1, []) of
        {<<>>, _Rest1, _Pos2} -> error;
        {Part, Rest1, Pos2} -> take_langtag_parts(Rest1, Pos2, [Part | Acc])
    end;
take_langtag_parts(Rest, Pos, Acc) ->
    {ok, lists:reverse(Acc), Rest, Pos}.

take_ascii_letters(<<C, Rest/binary>>, Pos, Acc)
  when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) ->
    take_ascii_letters(Rest, advance(C, Pos), [C | Acc]);
take_ascii_letters(Rest, Pos, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Rest, Pos}.

take_ascii_alnum(<<C, Rest/binary>>, Pos, Acc)
  when (C >= $a andalso C =< $z) orelse
       (C >= $A andalso C =< $Z) orelse
       (C >= $0 andalso C =< $9) ->
    take_ascii_alnum(Rest, advance(C, Pos), [C | Acc]);
take_ascii_alnum(Rest, Pos, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Rest, Pos}.

scan_number(Query, Scanned, Pos, Sign) ->
    case take_number(Query, Sign) of
        {ok, Type, Lexeme, Rest} ->
            scan(Rest, [token(Type, Pos, Lexeme) | Scanned], advance_binary(Lexeme, Pos));
        error ->
            scan_operator(Query, Scanned, Pos)
    end.

take_number(Query, Sign) ->
    Patterns = [
        {double, <<"^(?:[+-]?(?:[0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.[0-9]+[eE][+-]?[0-9]+|[0-9]+[eE][+-]?[0-9]+))">>},
        {decimal, <<"^[+-]?[0-9]*\\.[0-9]+">>},
        {integer, <<"^[+-]?[0-9]+">>}
    ],
    take_number(Query, Sign, Patterns).

take_number(_Query, _Sign, []) ->
    error;
take_number(Query, Sign, [{BaseType, Pattern} | Patterns]) ->
    case re:run(Query, Pattern, [{capture, first, binary}]) of
        {match, [Lexeme]} ->
            Size = byte_size(Lexeme),
            <<Lexeme:Size/binary, Rest/binary>> = Query,
            {ok, signed_number_type(BaseType, Sign), Lexeme, Rest};
        nomatch ->
            take_number(Query, Sign, Patterns)
    end.

signed_number_type(Type, unsigned) -> Type;
signed_number_type(integer, $+) -> integer_positive;
signed_number_type(decimal, $+) -> decimal_positive;
signed_number_type(double, $+) -> double_positive;
signed_number_type(integer, $-) -> integer_negative;
signed_number_type(decimal, $-) -> decimal_negative;
signed_number_type(double, $-) -> double_negative.

scan_word_or_prefixed_name(Query, Scanned, Pos) ->
    {Word, Rest, Pos1, EndsInDot} = take_prefix_chars(Query, Pos, [], false),
    case Rest of
        <<$:, Rest1/binary>> when not EndsInDot ->
            scan_prefixed_name(Word, Rest1, Scanned, Pos, advance($:, Pos1));
        _ ->
            {Keyword, Rest1, Pos2} = split_keyword_word(Word, Rest, Pos),
            scan_keyword(Keyword, Rest1, Scanned, Pos, Pos2)
    end.

split_keyword_word(Word, Rest, Pos) ->
    case binary:match(Word, [<<".">>, <<"-">>]) of
        nomatch ->
            {Word, Rest, advance_binary(Word, Pos)};
        {Dot, 1} ->
            <<Keyword:Dot/binary, Tail/binary>> = Word,
            {Keyword, <<Tail/binary, Rest/binary>>, advance_binary(Keyword, Pos)}
    end.

take_prefix_chars(<<C/utf8, Rest/binary>>, Pos, Acc, EndsInDot) ->
    case is_pn_chars(C) orelse C =:= $. of
        true ->
            take_prefix_chars(Rest, advance(C, Pos), [<<C/utf8>> | Acc], C =:= $.);
        false ->
            {iolist_to_binary(lists:reverse(Acc)), <<C/utf8, Rest/binary>>, Pos, EndsInDot}
    end;
take_prefix_chars(<<>>, Pos, Acc, EndsInDot) ->
    {iolist_to_binary(lists:reverse(Acc)), <<>>, Pos, EndsInDot}.

scan_prefixed_name(Prefix, Rest, Scanned, Pos, PosAfterColon) ->
    Namespace = <<Prefix/binary, $:>>,
    case take_pn_local(Rest, PosAfterColon) of
        no ->
            scan(Rest, [token(pname_ns, Pos, Namespace) | Scanned], PosAfterColon);
        {ok, Local, Rest1, Pos1} ->
            Value = <<Namespace/binary, Local/binary>>,
            scan(Rest1, [token(pname_ln, Pos, Value) | Scanned], Pos1)
    end.

take_pn_local(Bin, Pos) ->
    case take_pn_local_unit(Bin, Pos, first) of
        no ->
            no;
        {ok, Value, Rest, Pos1, valid} ->
            Acc = [Value],
            take_pn_local_rest(Rest, Pos1, Acc, {Rest, Pos1, Acc})
    end.

take_pn_local_rest(Bin, Pos, Acc, Checkpoint) ->
    case take_pn_local_unit(Bin, Pos, rest) of
        no ->
            pn_local_checkpoint(Checkpoint);
        {ok, Value, Rest, Pos1, valid} ->
            Acc1 = [Value | Acc],
            take_pn_local_rest(Rest, Pos1, Acc1, {Rest, Pos1, Acc1});
        {ok, Value, Rest, Pos1, dot} ->
            take_pn_local_rest(Rest, Pos1, [Value | Acc], Checkpoint)
    end.

take_pn_local_unit(<<$%, H1, H2, Rest/binary>>, Pos, _Place)
  when ((H1 >= $0 andalso H1 =< $9) orelse (H1 >= $A andalso H1 =< $F) orelse (H1 >= $a andalso H1 =< $f)),
       ((H2 >= $0 andalso H2 =< $9) orelse (H2 >= $A andalso H2 =< $F) orelse (H2 >= $a andalso H2 =< $f)) ->
    Lexeme = <<$%, H1, H2>>,
    {ok, Lexeme, Rest, advance_binary(Lexeme, Pos), valid};
take_pn_local_unit(<<$\\, C, Rest/binary>>, Pos, _Place) ->
    case is_pn_local_escape(C) of
        true -> {ok, <<C>>, Rest, advance(C, advance($\\, Pos)), valid};
        false -> no
    end;
take_pn_local_unit(<<C/utf8, Rest/binary>>, Pos, first) ->
    case is_pn_chars_u(C) orelse is_digit(C) orelse C =:= $: of
        true -> {ok, <<C/utf8>>, Rest, advance(C, Pos), valid};
        false -> no
    end;
take_pn_local_unit(<<C/utf8, Rest/binary>>, Pos, rest) ->
    case {is_pn_chars(C) orelse C =:= $:, C =:= $.} of
        {true, _} -> {ok, <<C/utf8>>, Rest, advance(C, Pos), valid};
        {false, true} -> {ok, <<$.>>, Rest, advance(C, Pos), dot};
        _ -> no
    end;
take_pn_local_unit(<<>>, _Pos, _Place) ->
    no.

pn_local_checkpoint({Rest, Pos, Acc}) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), Rest, Pos}.

scan_keyword(<<"a">> = Word, Rest, Scanned, Pos, Pos1) ->
    scan(Rest, [token(a, Pos, Word) | Scanned], Pos1);
scan_keyword(Word, Rest, Scanned, Pos, Pos1) ->
    Upper = string:uppercase(Word),
    case keyword(Upper) of
        undefined ->
            {error, format_error(Pos, "Unknown keyword or invalid prefixed name ~ts", [Word])};
        Type ->
            scan(Rest, [token(Type, Pos, Word) | Scanned], Pos1)
    end.

%% SPARQL keywords are case-insensitive, except for the shorthand `a`.
keyword(<<"BASE">>) -> base;
keyword(<<"PREFIX">>) -> prefix;
keyword(<<"SELECT">>) -> select;
keyword(<<"DISTINCT">>) -> distinct;
keyword(<<"REDUCED">>) -> reduced;
keyword(<<"AS">>) -> as;
keyword(<<"CONSTRUCT">>) -> construct;
keyword(<<"WHERE">>) -> where;
keyword(<<"DESCRIBE">>) -> describe;
keyword(<<"ASK">>) -> ask;
keyword(<<"FROM">>) -> from;
keyword(<<"NAMED">>) -> named;
keyword(<<"GROUP">>) -> group;
keyword(<<"BY">>) -> by;
keyword(<<"HAVING">>) -> having;
keyword(<<"ORDER">>) -> order;
keyword(<<"ASC">>) -> asc;
keyword(<<"DESC">>) -> desc;
keyword(<<"LIMIT">>) -> limit;
keyword(<<"OFFSET">>) -> offset;
keyword(<<"VALUES">>) -> values;
keyword(<<"LOAD">>) -> load;
keyword(<<"SILENT">>) -> silent;
keyword(<<"INTO">>) -> into;
keyword(<<"CLEAR">>) -> clear;
keyword(<<"DROP">>) -> drop;
keyword(<<"CREATE">>) -> create;
keyword(<<"ADD">>) -> add;
keyword(<<"MOVE">>) -> move;
keyword(<<"COPY">>) -> copy;
keyword(<<"TO">>) -> to;
keyword(<<"INSERT">>) -> insert;
keyword(<<"DATA">>) -> data;
keyword(<<"DELETE">>) -> delete;
keyword(<<"WITH">>) -> with;
keyword(<<"USING">>) -> using;
keyword(<<"DEFAULT">>) -> default;
keyword(<<"GRAPH">>) -> graph;
keyword(<<"ALL">>) -> all;
keyword(<<"OPTIONAL">>) -> optional;
keyword(<<"SERVICE">>) -> service;
keyword(<<"BIND">>) -> bind;
keyword(<<"UNDEF">>) -> undef;
keyword(<<"MINUS">>) -> minus;
keyword(<<"UNION">>) -> union;
keyword(<<"FILTER">>) -> filter;
keyword(<<"NOT">>) -> 'not';
keyword(<<"EXISTS">>) -> exists;
keyword(<<"IN">>) -> in;
keyword(<<"STR">>) -> str;
keyword(<<"LANG">>) -> lang;
keyword(<<"LANGMATCHES">>) -> langmatches;
keyword(<<"DATATYPE">>) -> datatype;
keyword(<<"BOUND">>) -> bound;
keyword(<<"IRI">>) -> iri;
keyword(<<"URI">>) -> uri;
keyword(<<"BNODE">>) -> bnode;
keyword(<<"RAND">>) -> rand;
keyword(<<"ABS">>) -> abs;
keyword(<<"CEIL">>) -> ceil;
keyword(<<"FLOOR">>) -> floor;
keyword(<<"ROUND">>) -> round;
keyword(<<"CONCAT">>) -> concat;
keyword(<<"SUBSTR">>) -> substr;
keyword(<<"STRLEN">>) -> strlen;
keyword(<<"REPLACE">>) -> replace;
keyword(<<"UCASE">>) -> ucase;
keyword(<<"LCASE">>) -> lcase;
keyword(<<"ENCODE_FOR_URI">>) -> encode_for_uri;
keyword(<<"CONTAINS">>) -> contains;
keyword(<<"STRSTARTS">>) -> strstarts;
keyword(<<"STRENDS">>) -> strends;
keyword(<<"STRBEFORE">>) -> strbefore;
keyword(<<"STRAFTER">>) -> strafter;
keyword(<<"YEAR">>) -> year;
keyword(<<"MONTH">>) -> month;
keyword(<<"DAY">>) -> day;
keyword(<<"HOURS">>) -> hours;
keyword(<<"MINUTES">>) -> minutes;
keyword(<<"SECONDS">>) -> seconds;
keyword(<<"TIMEZONE">>) -> timezone;
keyword(<<"TZ">>) -> tz;
keyword(<<"NOW">>) -> now;
keyword(<<"UUID">>) -> uuid;
keyword(<<"STRUUID">>) -> struuid;
keyword(<<"MD5">>) -> md5;
keyword(<<"SHA1">>) -> sha1;
keyword(<<"SHA256">>) -> sha256;
keyword(<<"SHA384">>) -> sha384;
keyword(<<"SHA512">>) -> sha512;
keyword(<<"COALESCE">>) -> coalesce;
keyword(<<"IF">>) -> 'if';
keyword(<<"STRLANG">>) -> strlang;
keyword(<<"STRDT">>) -> strdt;
keyword(<<"SAMETERM">>) -> sameterm;
keyword(<<"ISIRI">>) -> isiri;
keyword(<<"ISURI">>) -> isuri;
keyword(<<"ISBLANK">>) -> isblank;
keyword(<<"ISLITERAL">>) -> isliteral;
keyword(<<"ISNUMERIC">>) -> isnumeric;
keyword(<<"REGEX">>) -> regex;
keyword(<<"COUNT">>) -> count;
keyword(<<"SUM">>) -> sum;
keyword(<<"MIN">>) -> min;
keyword(<<"MAX">>) -> max;
keyword(<<"AVG">>) -> avg;
keyword(<<"SAMPLE">>) -> sample;
keyword(<<"GROUP_CONCAT">>) -> group_concat;
keyword(<<"SEPARATOR">>) -> separator;
keyword(<<"TRUE">>) -> true;
keyword(<<"FALSE">>) -> false;
keyword(_) -> undefined.

%% NIL and ANON include their whitespace. Comments count as whitespace in
%% SPARQL, so they are accepted between the delimiters as well.
take_empty(<<Open, Rest/binary>>, Open, Close, Pos) ->
    Pos1 = advance(Open, Pos),
    take_empty_rest(Rest, Open, Close, Pos1, [<<Open>>]).

take_empty_rest(<<Close, Rest/binary>>, _Open, Close, Pos, Acc) ->
    Pos1 = advance(Close, Pos),
    Lexeme = iolist_to_binary(lists:reverse([<<Close>> | Acc])),
    {ok, Lexeme, Rest, Pos1};
take_empty_rest(<<$\s, Rest/binary>>, Open, Close, Pos, Acc) ->
    take_empty_rest(Rest, Open, Close, advance($\s, Pos), [<<$\s>> | Acc]);
take_empty_rest(<<$\t, Rest/binary>>, Open, Close, Pos, Acc) ->
    take_empty_rest(Rest, Open, Close, advance($\t, Pos), [<<$\t>> | Acc]);
take_empty_rest(<<"\r\n", Rest/binary>>, Open, Close, Pos, Acc) ->
    take_empty_rest(Rest, Open, Close, newline(Pos), [<<"\r\n">> | Acc]);
take_empty_rest(<<"\n", Rest/binary>>, Open, Close, Pos, Acc) ->
    take_empty_rest(Rest, Open, Close, newline(Pos), [<<"\n">> | Acc]);
take_empty_rest(<<"\r", Rest/binary>>, Open, Close, Pos, Acc) ->
    take_empty_rest(Rest, Open, Close, newline(Pos), [<<"\r">> | Acc]);
take_empty_rest(<<$#, Rest/binary>>, Open, Close, Pos, Acc) ->
    {Comment, Rest1, Pos1} = take_comment(Rest, advance($#, Pos), [<<$#>>]),
    take_empty_rest(Rest1, Open, Close, Pos1, [Comment | Acc]);
take_empty_rest(_Rest, _Open, _Close, _Pos, _Acc) ->
    no.

skip_comment(<<"\r\n", Rest/binary>>, Pos) -> {Rest, newline(Pos)};
skip_comment(<<"\n", Rest/binary>>, Pos) -> {Rest, newline(Pos)};
skip_comment(<<"\r", Rest/binary>>, Pos) -> {Rest, newline(Pos)};
skip_comment(<<C/utf8, Rest/binary>>, Pos) -> skip_comment(Rest, advance(C, Pos));
skip_comment(<<>>, Pos) -> {<<>>, Pos}.

take_comment(<<"\r\n", Rest/binary>>, Pos, Acc) ->
    {iolist_to_binary(lists:reverse([<<"\r\n">> | Acc])), Rest, newline(Pos)};
take_comment(<<"\n", Rest/binary>>, Pos, Acc) ->
    {iolist_to_binary(lists:reverse([<<"\n">> | Acc])), Rest, newline(Pos)};
take_comment(<<"\r", Rest/binary>>, Pos, Acc) ->
    {iolist_to_binary(lists:reverse([<<"\r">> | Acc])), Rest, newline(Pos)};
take_comment(<<C/utf8, Rest/binary>>, Pos, Acc) ->
    take_comment(Rest, advance(C, Pos), [<<C/utf8>> | Acc]);
take_comment(<<>>, Pos, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), <<>>, Pos}.

%%====================================================================
%% Character classes and source positions
%%====================================================================

is_digit(C) ->
    C >= $0 andalso C =< $9.

is_varname_start(C) ->
    is_pn_chars_u(C) orelse is_digit(C).

is_varname_char(C) ->
    is_pn_chars_u(C)
    orelse is_digit(C)
    orelse C =:= 16#00B7                            % middledot
    orelse (C >= 16#0300 andalso C =< 16#036F)      % diacriticals
    orelse C =:= 16#203F                            % undertie
    orelse C =:= 16#2040.                           % double undertie

is_pn_chars_u(C) -> is_pn_chars_base(C) orelse C =:= $_.

is_pn_chars(C) ->
    is_pn_chars_u(C)
    orelse C =:= $-
    orelse is_digit(C)
    orelse C =:= 16#00B7                            % middledot
    orelse (C >= 16#0300 andalso C =< 16#036F)      % diacriticals
    orelse C =:= 16#203F                            % undertie
    orelse C =:= 16#2040.                           % double undertie

is_pn_chars_base(C) when C >= $A, C =< $Z -> true;
is_pn_chars_base(C) when C >= $a, C =< $z -> true;
is_pn_chars_base(C) when C >= 16#00C0, C =< 16#00D6 -> true;
is_pn_chars_base(C) when C >= 16#00D8, C =< 16#00F6 -> true;
is_pn_chars_base(C) when C >= 16#00F8, C =< 16#02FF -> true;
is_pn_chars_base(C) when C >= 16#0370, C =< 16#037D -> true;
is_pn_chars_base(C) when C >= 16#037F, C =< 16#1FFF -> true;
is_pn_chars_base(C) when C >= 16#200C, C =< 16#200D -> true;
is_pn_chars_base(C) when C >= 16#2070, C =< 16#218F -> true;
is_pn_chars_base(C) when C >= 16#2C00, C =< 16#2FEF -> true;
is_pn_chars_base(C) when C >= 16#3001, C =< 16#D7FF -> true;
is_pn_chars_base(C) when C >= 16#F900, C =< 16#FDCF -> true;
is_pn_chars_base(C) when C >= 16#FDF0, C =< 16#FFFD -> true;
is_pn_chars_base(C) when C >= 16#10000, C =< 16#EFFFF -> true;
is_pn_chars_base(_) -> false.

is_pn_local_escape($_) -> true;
is_pn_local_escape($~) -> true;
is_pn_local_escape($.) -> true;
is_pn_local_escape($-) -> true;
is_pn_local_escape($!) -> true;
is_pn_local_escape($$) -> true;
is_pn_local_escape($&) -> true;
is_pn_local_escape($') -> true;
is_pn_local_escape($() -> true;
is_pn_local_escape($)) -> true;
is_pn_local_escape($*) -> true;
is_pn_local_escape($+) -> true;
is_pn_local_escape($,) -> true;
is_pn_local_escape($;) -> true;
is_pn_local_escape($=) -> true;
is_pn_local_escape($/) -> true;
is_pn_local_escape($?) -> true;
is_pn_local_escape($#) -> true;
is_pn_local_escape($@) -> true;
is_pn_local_escape($%) -> true;
is_pn_local_escape(_) -> false.

token(Type, Pos, Value) -> {Type, Pos, Value}.

advance_binary(Bin, Pos) ->
    advance_binary_1(Bin, Pos).

advance_binary_1(<<"\r\n", Rest/binary>>, Pos) ->
    advance_binary_1(Rest, newline(Pos));
advance_binary_1(<<"\n", Rest/binary>>, Pos) ->
    advance_binary_1(Rest, newline(Pos));
advance_binary_1(<<"\r", Rest/binary>>, Pos) ->
    advance_binary_1(Rest, newline(Pos));
advance_binary_1(<<C/utf8, Rest/binary>>, Pos) ->
    advance_binary_1(Rest, advance(C, Pos));
advance_binary_1(<<>>, Pos) ->
    Pos.

advance(_C, {SourceRef, Row, Column}) -> {SourceRef, Row, Column + 1}.
newline({SourceRef, Row, _Column}) -> {SourceRef, Row + 1, 1}.

format_error({SourceRef, Row, Column}, Format, Args) ->
    Message = io_lib:format(Format, Args),
    iolist_to_binary(io_lib:format("~tp:~p:~p: ~ts", [SourceRef, Row, Column, Message])).

%%====================================================================
%% SPARQL codepoint escapes
%%====================================================================

decode_codepoints(Bin) ->
    decode_codepoints(Bin, []).

decode_codepoints(<<$\\, $u, H1, H2, H3, H4, Rest/binary>>, Acc) ->
    decode_codepoint([H1, H2, H3, H4], Rest, Acc);
decode_codepoints(<<$\\, $U, H1, H2, H3, H4, H5, H6, H7, H8, Rest/binary>>, Acc) ->
    decode_codepoint([H1, H2, H3, H4, H5, H6, H7, H8], Rest, Acc);
decode_codepoints(<<C/utf8, Rest/binary>>, Acc) ->
    decode_codepoints(Rest, [<<C/utf8>> | Acc]);
decode_codepoints(<<>>, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
decode_codepoints(Invalid, _Acc) ->
    {error, iolist_to_binary(io_lib:format("Invalid UTF-8 input near ~tp", [Invalid]))}.

decode_codepoint(Hex, Rest, Acc) ->
    case lists:all(fun is_hex/1, Hex) of
        true ->
            Codepoint = list_to_integer(Hex, 16),
            case is_unicode_scalar(Codepoint) of
                true -> decode_codepoints(Rest, [<<Codepoint/utf8>> | Acc]);
                false ->
                    {error, iolist_to_binary(io_lib:format(
                        "Invalid Unicode codepoint escape \\u~s", [Hex]))}
            end;
        false ->
            {error, iolist_to_binary(io_lib:format("Invalid Unicode escape \\u~s", [Hex]))}
    end.

is_hex(C) when C >= $0, C =< $9 -> true;
is_hex(C) when C >= $A, C =< $F -> true;
is_hex(C) when C >= $a, C =< $f -> true;
is_hex(_) -> false.

is_unicode_scalar(C) ->
    C =< 16#10FFFF andalso not (C >= 16#D800 andalso C =< 16#DFFF).
