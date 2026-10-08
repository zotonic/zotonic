%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Decode, validate and execute SPARQL endpoint requests.
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

-module(z_sparql_protocol).
-moduledoc("Read-only SPARQL endpoint request decoding, validation and execution.").

-export([request/3, normalize/1, pagination/3, query/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec request(Method, ContentType, Context) -> {ok, map(), z:context()} | {error, term(), z:context()}
    when Method :: binary(), ContentType :: term(), Context :: z:context().
request(Method, ContentType, Context) ->
    try decode(Method, ContentType, Context) of
        {Payload, Context1} ->
            case normalize(Payload) of
                {ok, Request} -> {ok, Request, Context1};
                {error, Reason} -> {error, Reason, Context1}
            end
    catch
        error:_ -> {error, invalid_request, Context}
    end.

decode(<<"GET">>, _ContentType, Context) ->
    parameters(Context);
decode(<<"POST">>, {<<"application">>, <<"x-www-form-urlencoded">>, _}, Context) ->
    parameters(Context);
decode(<<"POST">>, {<<"application">>, <<"json">>, _}, Context) ->
    z_controller_helper:decode_request_noz({<<"application">>, <<"json">>, []}, Context);
decode(<<"POST">>, {<<"application">>, <<"sparql-query">>, _}, Context) ->
    {Body, Context1} = z_controller_helper:req_body(Context),
    {Params, Context2} = parameters(Context1),
    case maps:is_key(<<"query">>, Params) of
        true -> {invalid, Context2};
        false -> {Params#{<<"query">> => Body}, Context2}
    end.

parameters(Context) ->
    Context1 = z_context:ensure_qs(Context),
    Pairs = z_context:get_q_all_noz(Context1),
    % Reject duplicate fields instead of silently choosing one query or args map.
    Keys = [K || {K, _} <- Pairs],
    case length(Keys) =:= length(lists:usort(Keys)) of
        true -> {maps:from_list(Pairs), Context1};
        false -> {invalid, Context1}
    end.

-spec normalize(Payload) -> {ok, map()} | {error, atom()} when Payload :: term().
normalize(#{<<"query">> := Query} = Payload) when is_binary(Query), byte_size(Query) > 0 ->
    case maps:is_key(<<"default-graph-uri">>, Payload) orelse maps:is_key(<<"named-graph-uri">>, Payload) of
        true -> {error, unsupported_dataset};
        false ->
            case arguments(maps:get(<<"args">>, Payload, #{})) of
                {ok, Args} -> {ok, Payload#{<<"args">> => Args}};
                {error, _} = Error -> Error
            end
    end;
normalize(_) -> {error, invalid_request}.

arguments(Args) when is_map(Args) -> {ok, Args};
arguments(Json) when is_binary(Json) ->
    try z_json:decode(Json) of
        Args when is_map(Args) -> {ok, Args};
        _ -> {error, invalid_arguments}
    catch error:_ -> {error, invalid_arguments}
    end;
arguments(_) -> {error, invalid_arguments}.

%% HTTP paging is one-based; SPARQL OFFSET is zero-based. Supplying either
%% paging parameter selects HTTP paging and overrides both query modifiers.
-spec pagination(Request, Plan, Default) -> {ok, {pos_integer(), non_neg_integer()}} | {error, atom()}
    when Request :: map(), Plan :: map(), Default :: pos_integer().
pagination(Request, Plan, Default) ->
    case maps:is_key(<<"page">>, Request) orelse maps:is_key(<<"pagelen">>, Request) of
        true ->
            case {positive(default(maps:get(<<"page">>, Request, undefined), 1)),
                  positive(default(maps:get(<<"pagelen">>, Request, undefined), Default))} of
                {{ok, Page}, {ok, Length}} -> {ok, {(Page - 1) * Length + 1, Length}};
                _ -> {error, invalid_pagination}
            end;
        false ->
            Limit = default(maps:get(limit, Plan), Default),
            Offset = default(maps:get(offset, Plan), 0),
            {ok, {Offset + 1, Limit}}
    end.

positive(Value) when is_integer(Value), Value > 0 -> {ok, Value};
positive(Value) when is_binary(Value) ->
    try positive(binary_to_integer(Value)) catch error:_ -> error end;
positive(_) -> error.

default(undefined, Default) -> Default;
default(null, Default) -> Default;
default(<<>>, Default) -> Default;
default(Value, _Default) -> Value.

-spec query(Request, Context) -> {ok, map()} | {error, term()}
    when Request :: map(), Context :: z:context().
query(#{<<"query">> := Text, <<"args">> := Args} = Request, Context) ->
    case z_sparql:parse(Text) of
        {ok, Parsed} ->
            case z_sparql_plan:to_query_plan(Parsed, Args, Context) of
                {ok, Plan} -> run(Request, Plan, Context);
                {error, _} = Error -> Error
            end;
        {error, _} -> {error, malformed_query}
    end.

run(Request, Plan, Context) ->
    case pagination(Request, Plan, z_search:default_pagelen(Context)) of
        {ok, OffsetLimit} ->
            case z_sparql_sql:result_plan_to_sql(Plan#{limit => undefined, offset => undefined}, Context) of
                {ok, Variables, Terms} ->
                    Sql = z_search_terms:combine(Terms, Context),
                    #search_result{result = Rows} = z_search:search_result(Sql, OffsetLimit, Context),
                    {ok, z_sparql_results:document(Variables, Rows, Context)};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.
