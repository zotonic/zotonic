%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Serve SPARQL SELECT queries over HTTP with normal Zotonic ACL checks.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(controller_sparql).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "controller", "api_and_integration",
        "http", "rdf_and_linked_data", "authorization_and_access_control"
    ]
}).
-moduledoc("
Serve read-only SPARQL SELECT queries at `/sparql` using the caller's resource
and property ACLs. Requires `mod_sparql`; existing Zotonic model APIs are unchanged.

## Translation selection

Use `zotonic:translation(?title, \"en\")` for an exact translation and
`zotonic:translationFallback(?title, \"nl\")` for the canonical language and its
Zotonic fallback chain, then site default, English, and any available translation. Plain text stays untagged; missing values
stay unbound. Returned translations carry their actual language tag.

## Requests

| Method | Content type | Query location |
| --- | --- | --- |
| GET | — | URL-encoded `query` parameter. |
| POST | `application/x-www-form-urlencoded` | URL-encoded `query` field. |
| POST | `application/sparql-query` | Raw UTF-8 SPARQL body. |
| POST | `application/json` | JSON object containing `query`. |

JSON requests and the `args`, `page` and `pagelen` fields are Zotonic extensions.
For raw SPARQL POST, supply extension fields in the URL. For JSON POST, supply
fields in the body. For GET and form POST, use request parameters.

`args` binds named variables without interpolating query text. Pass a nested
object in JSON requests, or a JSON-encoded object in URL/form parameters.
Names omit the leading `?`; values follow the typed argument format in `m_sparql`.

Example JSON request body:

```json
{
  \"query\": \"SELECT ?article WHERE { ?article :author ?author } ORDER BY ?article LIMIT 20\",
  \"args\": {\"author\": {\"type\": \"resource\", \"value\": 123}}
}
```

## Paging

If `page` or `pagelen` is present, HTTP paging overrides both query LIMIT and
OFFSET. Missing or empty fields (empty strings or JSON null) default to page 1
and the site's search page length. Nonempty values must be positive integers.

With neither field present, query LIMIT/OFFSET apply. Missing clauses default
to the site's search page length and offset 0. LIMIT 0 returns no rows.

## Results

Select the output format with the HTTP `Accept` header:

| Accept | Result format |
| --- | --- |
| `application/sparql-results+json` | SPARQL Results JSON (default). |
| `application/json` | Alias for SPARQL Results JSON. |
| `application/sparql-results+xml` | SPARQL Results XML. |
| `text/csv` | CSV with variable names in the header. |
| `text/tab-separated-values` | TSV with `?variable` headers and RDF term syntax. |

JSON, XML and TSV preserve RDF term metadata. CSV returns plain values and
cannot distinguish empty strings from unbound values; datatype and language
tags are omitted. CSV uses CRLF line endings; TSV uses LF. Output uses UTF-8.

In JSON, `head.vars` lists variables and `results.bindings` contains named RDF terms.
Resource bindings use IRIs; literals carry datatype or `xml:lang` metadata.
Unbound variables are omitted from their row. Empty results retain `head.vars`.

Selected `#trans{}` records use context-language fallback and return text tagged
with the language actually selected. Empty records and undefined values are
unbound; binary strings remain literals. This implicit lookup happens after SQL
filtering, ordering, grouping and paging; explicit translation functions run in SQL.
Responses are not cached.

## Errors and limits

- 400: invalid requests, malformed queries or unsupported query features.
- 406: unsupported response format.
- 415: unsupported request content type.
- 422: a selected value has no supported RDF representation.
- 500: query execution failed; database details are not returned.

Controller-generated errors use JSON with an `error` code.
Only the SELECT subset documented in `mod_sparql` is supported. Variable
predicates, dataset selection, ASK, CONSTRUCT, DESCRIBE and updates are unsupported.
RDF graph output formats are not implemented. Errors remain JSON in every format.
").

-export([service_available/1, allowed_methods/1, content_types_accepted/1,
         content_types_provided/1, process/4]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec service_available(z:context()) -> {boolean(), z:context()}.
service_available(Context) ->
    {true, z_context:set_nocache_headers(z_context:set_noindex_header(true, Context))}.

-spec allowed_methods(z:context()) -> {[binary()], z:context()}.
allowed_methods(Context) -> {[<<"GET">>, <<"POST">>], Context}.

-spec content_types_accepted(z:context()) -> {[cowmachine_req:media_type()], z:context()}.
content_types_accepted(Context) ->
    {[{<<"application">>, <<"sparql-query">>, []},
      {<<"application">>, <<"x-www-form-urlencoded">>, []},
      {<<"application">>, <<"json">>, []}], Context}.

-spec content_types_provided(z:context()) -> {[cowmachine_req:media_type()], z:context()}.
content_types_provided(Context) ->
    {[{<<"application">>, <<"sparql-results+json">>, []},
      {<<"application">>, <<"json">>, []},
      {<<"application">>, <<"sparql-results+xml">>, []},
      {<<"text">>, <<"csv">>, []},
      {<<"text">>, <<"tab-separated-values">>, []}], Context}.

-spec process(binary(), term(), term(), z:context()) ->
    {binary(), z:context()} | {{halt, pos_integer()}, z:context()}.
process(Method, Accepted, Provided, Context) ->
    case z_sparql_protocol:request(Method, Accepted, Context) of
        {ok, Request, Context1} -> execute(Request, Provided, Context1);
        {error, Reason, Context1} -> error_response(400, Reason, Context1)
    end.

execute(Request, Provided, Context) ->
    try z_sparql_protocol:query(Request, Context) of
        {ok, Document} -> {z_sparql_results_encode:encode(Provided, Document), Context};
        {error, Reason} -> error_response(400, Reason, Context)
    catch
        throw:{error, unsupported_result_term} ->
            error_response(422, unsupported_result_term, Context);
        Class:_Reason ->
            % Database exceptions can contain SQL and private values. Do not
            % expose them in the response or log the query/argument payload.
            ?LOG_ERROR(#{in => zotonic_mod_sparql, text => <<"SPARQL endpoint execution failed">>,
                         result => error, reason => query_execution_failed, class => Class}),
            error_response(500, query_execution_failed, Context)
    end.

error_response(Status, Reason, Context) ->
    Code = case Reason of
        R when is_atom(R) -> atom_to_binary(R, utf8);
        {unsupported, _} -> <<"unsupported_query_feature">>;
        {unsupported_query, _} -> <<"unsupported_query_form">>;
        {not_selectable, _} -> <<"not_selectable">>;
        _ -> <<"invalid_query">>
    end,
    Body = z_json:encode(#{<<"error">> => Code}),
    Context1 = z_context:set_resp_header(<<"content-type">>, <<"application/json">>, Context),
    {{halt, Status}, cowmachine_req:set_resp_body(Body, Context1)}.
