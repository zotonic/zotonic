%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Map SPARQL 1.1 built-in functions to PostgreSQL expressions.
%%
%% Only functions which have a matching SQL expression with (very) similar
%% enough semantics are supported.
%%
%% Functions inspecting RDF term metadata, such as LANG, DATATYPE and isIRI,
%% need typed bindings and are not yet added. isLITERAL, isNUMERIC and sameTerm
%% use the PostgreSQL scalar type as a useful approximation.
%%
%% SHA hashes and UUID functions are omitted as they depend on PostgreSQL
%% extensions or version-specific functions.
%%
%% REPLACE supports a subset of SPARQL XPath and PostgreSQL regexps.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(z_sparql_sql_function).

-export([
    to_sql/2,
    type_signature/2
]).

-type sql_expression() :: term().

% The 'common' type is used if two arguments can be of 'any' type, but
% must resolve to the same type.
-type result_type() ::
      boolean
    | datetime
    | float
    | integer
    | number
    | text
    | common.

-type argument_type() ::
      any
    | result_type().

-spec to_sql(Function, Arguments) -> {ok, sql_expression()} | {error, Reason} when
    Function :: atom() | term(),
    Arguments :: [ sql_expression() ],
    Reason :: {invalid_function_arity, atom(), non_neg_integer()}
        | {unsupported_function, term()}.
to_sql(bound, [Value]) ->
    {ok, [<<"(">>, Value, <<" IS NOT NULL)">>]};
to_sql(abs, [Value]) ->
    sql_call(<<"abs">>, [Value]);
to_sql(ceil, [Value]) ->
    sql_call(<<"ceil">>, [Value]);
to_sql(floor, [Value]) ->
    sql_call(<<"floor">>, [Value]);
to_sql(round, [Value]) ->
    sql_call(<<"round">>, [Value]);
to_sql(concat, []) ->
    {ok, <<"CAST('' AS text)">>};
to_sql(concat, Arguments) when is_list(Arguments) ->
    sql_call(<<"concat">>, Arguments);
to_sql(substr, [Value, Start]) ->
    sql_call(<<"substr">>, [Value, Start]);
to_sql(substr, [Value, Start, Length]) ->
    sql_call(<<"substr">>, [Value, Start, Length]);
to_sql(strlen, [Value]) ->
    sql_call(<<"char_length">>, [Value]);
to_sql(ucase, [Value]) ->
    sql_call(<<"upper">>, [Value]);
to_sql(lcase, [Value]) ->
    sql_call(<<"lower">>, [Value]);
to_sql(contains, [Value, Substring]) ->
    {ok, [<<"(strpos(">>, Value, <<", ">>, Substring, <<") > 0)">>]};
to_sql(strstarts, [Value, Prefix]) ->
    sql_call(<<"starts_with">>, [Value, Prefix]);
to_sql(strends, [Value, Suffix]) ->
    {ok, [
        <<"starts_with(reverse(">>, Value, <<"), reverse(">>, Suffix, <<"))">>
    ]};
to_sql(strbefore, [Value, Delimiter]) ->
    strbefore(Value, Delimiter);
to_sql(strafter, [Value, Delimiter]) ->
    strafter(Value, Delimiter);
to_sql(year, [Value]) ->
    extract(<<"YEAR">>, Value);
to_sql(month, [Value]) ->
    extract(<<"MONTH">>, Value);
to_sql(day, [Value]) ->
    extract(<<"DAY">>, Value);
to_sql(hours, [Value]) ->
    extract(<<"HOUR">>, Value);
to_sql(minutes, [Value]) ->
    extract(<<"MINUTE">>, Value);
to_sql(seconds, [Value]) ->
    extract(<<"SECOND">>, Value);
to_sql(now, []) ->
    {ok, <<"CURRENT_TIMESTAMP">>};
to_sql(rand, []) ->
    sql_call(<<"random">>, []);
to_sql(md5, [Value]) ->
    sql_call(<<"md5">>, [Value]);
to_sql(coalesce, [_ | _] = Arguments) ->
    sql_call(<<"coalesce">>, Arguments);
to_sql('if', [Condition, Then, Else]) ->
    {ok, [
        <<"(CASE WHEN ">>, Condition,
        <<" THEN ">>, Then,
        <<" ELSE ">>, Else,
        <<" END)">>
    ]};
% %% PostgreSQL scalars and JSONB scalars are normalized to JSONB so that one
% %% check handles both storage forms. Objects and arrays are structured Zotonic
% %% values and are not considered RDF literals by this approximation.
% %% If the types were already known, then z_sparql_sql.erl directly emits
% %% optimized code.
% to_sql(isliteral, [Value]) ->
%     {ok, [
%         <<"(jsonb_typeof(to_jsonb(">>, Value, <<")) IN ('string', 'number', 'boolean'))">>
%     ]};
% to_sql(isnumeric, [Value]) ->
%     {ok, [
%         <<"(jsonb_typeof(to_jsonb(">>, Value, <<")) = 'number')">>
%     ]};
% to_sql(sameterm, [Left, Right]) ->
%     {ok, [
%         <<"(jsonb_typeof(to_jsonb(">>, Left, <<")) IN ('string', 'number', 'boolean') ">>,
%         <<"AND jsonb_typeof(to_jsonb(">>, Left, <<")) = jsonb_typeof(to_jsonb(">>, Right, <<")) ">>,
%         <<"AND (to_jsonb(">>, Left, <<"))::text = (to_jsonb(">>, Right, <<"))::text)">>
%     ]};
to_sql(regex, [Value, Pattern]) ->
    regex(Value, Pattern, undefined);
to_sql(regex, [Value, Pattern, Flags]) ->
    regex(Value, Pattern, Flags);
to_sql(replace, [Value, Pattern, Replacement]) ->
    replace(Value, Pattern, Replacement, <<"'g'">>);
to_sql(replace, [Value, Pattern, Replacement, Flags]) ->
    replace(Value, Pattern, Replacement, [ <<"concat(">>, Flags, <<", 'g')">> ]);
to_sql(Function, Arguments) when is_atom(Function) ->
    case is_supported(Function) of
        true ->
            {error, {invalid_function_arity, Function, length(Arguments)}};
        false ->
            {error, {unsupported_function, Function}}
    end;
to_sql(Function, _Arguments) ->
    {error, {unsupported_function, Function}}.

%% @doc Return the SPARQL input and output types for a supported builtin.
%% The SQL generator resolves number and common against the actual argument
%% types before adding casts. `any` deliberately preserves the storage form.
-spec type_signature(Function, Arity) ->
    {ok, {[ argument_type() ], result_type()}}
    | {error, {invalid_function_arity, atom(), non_neg_integer()}}
    | {error, {unsupported_function, term()}}
when
    Function :: atom() | term(),
    Arity :: non_neg_integer().
type_signature(bound, 1) -> {ok, {[any], boolean}};
type_signature(abs, 1) -> {ok, {[number], number}};
type_signature(ceil, 1) -> {ok, {[number], number}};
type_signature(floor, 1) -> {ok, {[number], number}};
type_signature(round, 1) -> {ok, {[number], number}};
type_signature(concat, Arity) -> {ok, {lists:duplicate(Arity, text), text}};
type_signature(substr, 2) -> {ok, {[text, integer], text}};
type_signature(substr, 3) -> {ok, {[text, integer, integer], text}};
type_signature(strlen, 1) -> {ok, {[text], integer}};
type_signature(ucase, 1) -> {ok, {[text], text}};
type_signature(lcase, 1) -> {ok, {[text], text}};
type_signature(contains, 2) -> {ok, {[text, text], boolean}};
type_signature(strstarts, 2) -> {ok, {[text, text], boolean}};
type_signature(strends, 2) -> {ok, {[text, text], boolean}};
type_signature(strbefore, 2) -> {ok, {[text, text], text}};
type_signature(strafter, 2) -> {ok, {[text, text], text}};
type_signature(year, 1) -> {ok, {[datetime], integer}};
type_signature(month, 1) -> {ok, {[datetime], integer}};
type_signature(day, 1) -> {ok, {[datetime], integer}};
type_signature(hours, 1) -> {ok, {[datetime], integer}};
type_signature(minutes, 1) -> {ok, {[datetime], integer}};
type_signature(seconds, 1) -> {ok, {[datetime], float}};
type_signature(now, 0) -> {ok, {[], datetime}};
type_signature(rand, 0) -> {ok, {[], float}};
type_signature(md5, 1) -> {ok, {[text], text}};
type_signature(coalesce, Arity) when Arity > 0 -> {ok, {lists:duplicate(Arity, common), common}};
type_signature('if', 3) -> {ok, {[boolean, common, common], common}};
type_signature(isliteral, 1) -> {ok, {[any], boolean}};
type_signature(isnumeric, 1) -> {ok, {[any], boolean}};
type_signature(sameterm, 2) -> {ok, {[any, any], boolean}};
type_signature(regex, 2) -> {ok, {[text, text], boolean}};
type_signature(regex, 3) -> {ok, {[text, text, text], boolean}};
type_signature(replace, 3) -> {ok, {[text, text, text], text}};
type_signature(replace, 4) -> {ok, {[text, text, text, text], text}};
type_signature(Function, Arity) when is_atom(Function) ->
    case is_supported(Function) of
        true -> {error, {invalid_function_arity, Function, Arity}};
        false -> {error, {unsupported_function, Function}}
    end;
type_signature(Function, _Arity) ->
    {error, {unsupported_function, Function}}.

sql_call(Name, Arguments) ->
    {ok, [Name, $\(, lists:join(<<", ">>, Arguments), $\)]}.

extract(Field, Value) ->
    {ok, [<<"EXTRACT(">>, Field, <<" FROM ">>, Value, $\)]}.

%% @doc PostgreSQL split_part returns the complete value if the delimiter is not
%% found. SPARQL STRBEFORE returns an empty string. The CASE also implements the
%% SPARQL empty-delimiter rule, like with strpos(Value, '').
strbefore(Value, Delimiter) ->
    {ok, [
        <<"(CASE WHEN strpos(">>, Value, <<", ">>, Delimiter, <<") = 0 ">>,
        <<"THEN '' ELSE left(">>, Value, <<", strpos(">>, Value, <<", ">>,
        Delimiter, <<") - 1) END)">>
    ]}.

%% @doc PostgreSQL split_part only returns the second field, but SPARQL
%% STRAFTER returns everything following the first delimiter. We use strpos
%% to be compatible with the SPARQL semantics for missing and empty delimiters.
strafter(Value, Delimiter) ->
    {ok, [
        <<"(CASE WHEN strpos(">>, Value, <<", ">>, Delimiter, <<") = 0 ">>,
        <<"THEN '' ELSE substr(">>, Value, <<", strpos(">>, Value, <<", ">>,
        Delimiter, <<") + char_length(">>, Delimiter, <<")) END)">>
    ]}.

%% @doc SPARQL REGEX uses XPath regexps, where PostgreSQL uses POSIX.
%% The i/m/s/x flags and common pattern syntax form a useful subset.
%% PostgreSQL's ~ operator has no flags argument, so flags are prepended to the
%% pattern using its embedded-option syntax.
regex(Value, Pattern, undefined) ->
    {ok, [<<"(">>, Value, <<" ~ ">>, Pattern, <<")">>]};
regex(Value, Pattern, Flags) ->
    {ok, [
        <<"(">>, Value, <<" ~ concat('(?', ">>, Flags, <<", ')', ">>, Pattern, <<"))">>
    ]}.

%% @doc SPARQL REPLACE uses XPath regular expressions and replaces all matches.
%% PostgreSQL uses POSIX ARE expressions and replaces only the first match by
%% default. This mapping supports patterns common to both dialects, forces the
%% PostgreSQL global flag, and maps SPARQL $0..$9 capture references below.
replace(Value, Pattern, Replacement, Flags) ->
    Replacement1 = replacement_expression(Replacement),
    {ok, [
        <<"regexp_replace(">>, Value, <<", ">>, Pattern, <<", ">>, Replacement1, <<", ">>, Flags, $\)
    ]}.

%% @doc XPath uses $0 for the complete match and $1..$9 for captured groups.
%% PostgreSQL uses \& and \1..\9. chr(92) is used to avoid problems with the
%% psql string configs. Note that literal '$' and backslash replacements are
%% not supported, they would need a full XPath implementation and/or parser.
replacement_expression(Replacement) ->
    [
        <<"replace(replace(">>, Replacement, <<", '$0', chr(92) || '&'), '$', chr(92))">>
    ].

is_supported(abs) -> true;
is_supported(ceil) -> true;
is_supported(floor) -> true;
is_supported(round) -> true;
is_supported(concat) -> true;
is_supported(substr) -> true;
is_supported(strlen) -> true;
is_supported(ucase) -> true;
is_supported(lcase) -> true;
is_supported(contains) -> true;
is_supported(strstarts) -> true;
is_supported(strends) -> true;
is_supported(strbefore) -> true;
is_supported(strafter) -> true;
is_supported(year) -> true;
is_supported(month) -> true;
is_supported(day) -> true;
is_supported(hours) -> true;
is_supported(minutes) -> true;
is_supported(seconds) -> true;
is_supported(now) -> true;
is_supported(rand) -> true;
is_supported(md5) -> true;
is_supported(coalesce) -> true;
is_supported('if') -> true;
is_supported(bound) -> true;
is_supported(isliteral) -> true;
is_supported(isnumeric) -> true;
is_supported(sameterm) -> true;
is_supported(regex) -> true;
is_supported(replace) -> true;
is_supported(_) -> false.
