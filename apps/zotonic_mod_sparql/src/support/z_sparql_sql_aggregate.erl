%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Map SPARQL 1.1 aggregates to PostgreSQL aggregate expressions.
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

-module(z_sparql_sql_aggregate).

-export([
    to_sql/5,
    type_signature/1
]).

-type aggregate() ::
     count
   | sum
   | min
   | max
   | avg
   | sample
   | group_concat.

-type distinct() ::
     default
   | distinct.

-type sql_expression() :: term().

-type value_type() ::
      any
    | common
    | float
    | integer
    | number
    | text.

-spec to_sql(Aggregate, Distinct, Argument, Separator, Context) ->
    {ok, sql_expression()} | {error, term()}
when
    Aggregate :: aggregate() | term(),
    Distinct :: distinct(),
    Argument :: all | sql_expression(),
    Separator :: undefined | sql_expression(),
    Context :: z:context().
to_sql(count, default, all, undefined, _Context) ->
    {ok, <<"count(*)">>};
to_sql(count, distinct, all, undefined, _Context) ->
    {error, {unsupported, count_distinct_star}};
to_sql(count, Distinct, Argument, undefined, _Context) ->
    aggregate_call(<<"count">>, Distinct, Argument);
to_sql(sum, Distinct, Argument, undefined, _Context) ->
    coalesce_aggregate(<<"sum">>, Distinct, Argument, <<"0">>);
to_sql(min, Distinct, Argument, undefined, _Context) ->
    aggregate_call(<<"min">>, Distinct, Argument);
to_sql(max, Distinct, Argument, undefined, _Context) ->
    aggregate_call(<<"max">>, Distinct, Argument);
to_sql(avg, Distinct, Argument, undefined, _Context) ->
    coalesce_aggregate(<<"avg">>, Distinct, Argument, <<"0">>);
to_sql(group_concat, Distinct, Argument, undefined, _Context) ->
    group_concat(Distinct, Argument, <<"' '">>);
to_sql(group_concat, Distinct, Argument, Separator, _Context) ->
    group_concat(Distinct, Argument, Separator);
to_sql(sample, Distinct, Argument, undefined, Context) ->
    sample(Distinct, Argument, Context);
to_sql(Aggregate, _Distinct, _Argument, _Separator, _Context) ->
    {error, {unsupported_aggregate, Aggregate}}.

-spec type_signature(Aggregate) ->
    {ok, {value_type(), value_type()}} | {error, {unsupported_aggregate, term()}}
when
    Aggregate :: aggregate() | term().
type_signature(count) -> {ok, {any, integer}};
type_signature(sum) -> {ok, {number, number}};
type_signature(min) -> {ok, {common, common}};
type_signature(max) -> {ok, {common, common}};
type_signature(avg) -> {ok, {number, float}};
type_signature(sample) -> {ok, {any, common}};
type_signature(group_concat) -> {ok, {text, text}};
type_signature(Aggregate) -> {error, {unsupported_aggregate, Aggregate}}.

aggregate_call(Name, default, Argument) ->
    {ok, [Name, $\(, Argument, $\)]};
aggregate_call(Name, distinct, Argument) ->
    {ok, [Name, <<"(DISTINCT ">>, Argument, $\)]}.

coalesce_aggregate(Name, Distinct, Argument, Default) ->
    {ok, Aggregate} = aggregate_call(Name, Distinct, Argument),
    {ok, [<<"coalesce(">>, Aggregate, <<", ">>, Default, $\)]}.

group_concat(Distinct, Argument, Separator) ->
    Argument1 = distinct_argument(Distinct, Argument),
    {ok, [
        <<"coalesce(string_agg(">>, Argument1, <<", ">>, Separator,
        <<"), CAST('' AS text))">>
    ]}.

distinct_argument(default, Argument) -> Argument;
distinct_argument(distinct, Argument) -> [<<"DISTINCT ">>, Argument].

%% @doc PostgreSQL added any_value() in version 16. Earlier releases do not
%% have a direct equivalent with the unspecified-value semantics of SPARQL SAMPLE.
%% Instead of working around it, just throw an error for older PostgreSQL versions.
sample(Distinct, Argument, Context) ->
    case z_db:database_version(Context) of
        {ok, {postgresql, Major, _Minor}} when Major >= 16 ->
            aggregate_call(<<"any_value">>, Distinct, Argument);
        {ok, {postgresql, Major, _Minor}} ->
            {error, {unsupported_postgresql_version, sample, Major}};
        {ok, {Database, _Major, _Minor}} ->
            {error, {unsupported_database, sample, Database}};
        {error, Reason} ->
            {error, {database_version, Reason}}
    end.
