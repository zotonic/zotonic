%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Map well-known RDF datatype constructor IRIs to PostgreSQL types.
%%
%% The complete IRI is used deliberately. Query prefixes are expanded by the
%% query planner before expressions reach this mapping via the SQL generator.
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

-module(z_sparql_sql_datatype).

-export([
    mapping/1,
    datatype_type/1
]).

-type value_type() ::
      boolean
    | datetime
    | float
    | integer
    | number
    | text
    | uri.

-type sql_type() :: binary().

-export_type([
    value_type/0
]).

%% @doc Map xsd types to PostgreSQL types.
%% We use the full IRI as in sparql different prefixes can be used.
%% For xsd types, see: https://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes
%% For numeric psql types, see: https://www.postgresql.org/docs/current/datatype-numeric.html
-spec mapping(Iri) -> {ok, {ValueType, SqlType}} | undefined
    when
        Iri :: binary(),
        ValueType :: value_type(),
        SqlType :: sql_type().
mapping(<<"http://www.w3.org/2001/XMLSchema#string">>) ->
    {ok, {text, <<"text">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#boolean">>) ->
    {ok, {boolean, <<"boolean">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#integer">>) ->
    {ok, {integer, <<"bigint">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#long">>) ->
    {ok, {integer, <<"bigint">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#int">>) ->
    {ok, {integer, <<"integer">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#short">>) ->
    {ok, {integer, <<"smallint">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#decimal">>) ->
    %% TODO: We might want to map xsd:decimal to double, for better compatibility with Erlang.
    %% Though xsd:decimal should have 18 digits of precision, where the PostgreSQL
    %% double has only 15.
    {ok, {number, <<"numeric">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#float">>) ->
    {ok, {float, <<"real">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#double">>) ->
    {ok, {float, <<"double precision">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#dateTime">>) ->
    {ok, {datetime, <<"timestamptz">>}};
mapping(<<"http://www.w3.org/2001/XMLSchema#dateTimeStamp">>) ->
    {ok, {datetime, <<"timestamptz">>}};
mapping(_Iri) ->
    undefined.

%% @doc Return the expression value type for an RDF datatype IRI.
%% For now we support the xsd built-in primitive types.
%% TODO: check if we need to support other types as well (notification needed?)
-spec datatype_type(Datatype) -> value_type()
    when Datatype :: binary() | undefined.
datatype_type(undefined) -> text;
datatype_type(<<"http://www.w3.org/2001/XMLSchema#", Name/binary>>) -> xsd_type(Name);
datatype_type(_Datatype) -> text.

% See: https://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes
xsd_type(Name)
    when Name =:= <<"integer">>;
         Name =:= <<"long">>;
         Name =:= <<"int">>;
         Name =:= <<"short">>;
         Name =:= <<"byte">>;
         Name =:= <<"nonPositiveInteger">>;
         Name =:= <<"negativeInteger">>;
         Name =:= <<"nonNegativeInteger">>;
         Name =:= <<"positiveInteger">>;
         Name =:= <<"unsignedLong">>;
         Name =:= <<"unsignedInt">>;
         Name =:= <<"unsignedShort">>;
         Name =:= <<"unsignedByte">> -> integer;
xsd_type(Name)
    when Name =:= <<"decimal">>;
         Name =:= <<"double">>;
         Name =:= <<"float">> -> float;
xsd_type(<<"boolean">>) -> boolean;
xsd_type(Name)
    when Name =:= <<"dateTime">>;
         Name =:= <<"dateTimeStamp">>;
         Name =:= <<"date">> -> datetime;
xsd_type(<<"anyURI">>) -> uri;
xsd_type(_Name) -> text.
