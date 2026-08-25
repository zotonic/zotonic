%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Classification and compilation of stored query-resource text.
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

-module(search_query_resource).
-moduledoc("
Classify and compile query-resource text through the `search_query_parse`
notification. The resulting SQL search terms are executed by the normal search
pipeline, which adds resource ACL restrictions before running the SQL.

The saved `query_type` is a parser hint and is always checked server-side. If it
is absent, active query-language modules classify the text. Query resources are
loaded through the ACL-aware resource model; invisible resources are reported
as not found.
").

-export([
    parse/2,
    parse/4,
    from_resource/3,
    compile/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type parsed_query() :: #{
    query_type := binary(),
    query_type_label := binary() | #trans{},
    parsed := term(),
    search_terms := #search_sql_terms{} | #search_result{},
    is_live := boolean(),
    show_parsed := boolean()
}.

-type query_parse_error() :: #{
    query_type := binary() | undefined,
    query_type_label := binary() | #trans{},
    is_live := boolean(),
    show_parsed := boolean(),
    reason := term()
}.

-export_type([ parsed_query/0, query_parse_error/0 ]).


-spec parse(Query, Context) -> {ok, Parsed} | {error, Reason}
    when
        Query :: binary() | string(),
        Context :: z:context(),
        Parsed :: parsed_query(),
        Reason :: term().
parse(Query, Context) ->
    parse(Query, undefined, #{}, Context).

-spec parse(Query, QueryType, Arguments, Context) -> {ok, Parsed} | {error, Reason}
    when
        Query :: binary() | string(),
        QueryType :: binary() | atom() | undefined,
        Arguments :: map(),
        Context :: z:context(),
        Parsed :: parsed_query(),
        Reason :: term().
parse(Query, QueryType, Arguments, Context)
        when (is_binary(Query) orelse is_list(Query)), is_map(Arguments) ->
    Query1 = z_string:trim(unicode:characters_to_binary(Query, utf8)),
    QueryType1 = normalize_query_type(QueryType),
    case Query1 of
        <<>> ->
            query_parse_error(QueryType1, empty_query, Context);
        _ ->
            Notification = #search_query_parse{
                query = Query1,
                query_type = QueryType1,
                arguments = Arguments
            },
            Result = case QueryType1 of
                undefined ->
                    z_search:with_query_check(
                        fun() -> z_notifier:first(Notification, Context) end);
                _ ->
                    z_notifier:first(Notification, Context)
            end,
            validate_result(Result, QueryType1, Context)
    end;
parse(_Query, QueryType, _Arguments, Context) ->
    query_parse_error(normalize_query_type(QueryType), invalid_arguments, Context).

-spec from_resource(Id, Arguments, Context) -> {ok, Parsed} | {error, Reason}
    when
        Id :: m_rsc:resource(),
        Arguments :: map(),
        Context :: z:context(),
        Parsed :: parsed_query(),
        Reason :: term().
from_resource(Id, Arguments, Context) when is_map(Arguments) ->
    case m_rsc:rid(Id, Context) of
        RscId when is_integer(RscId) ->
            from_resource_1(RscId, Arguments, Context);
        undefined ->
            {error, enoent}
    end;
from_resource(_Id, _Arguments, _Context) ->
    {error, invalid_arguments}.

from_resource_1(RscId, Arguments, Context) ->
    case m_rsc:is_visible(RscId, Context)
        andalso m_rsc:is_a(RscId, query, Context)
    of
        true ->
            case m_rsc:p(RscId, <<"query">>, Context) of
                Query when is_binary(Query); is_list(Query) ->
                    QueryType = m_rsc:p(RscId, <<"query_type">>, Context),
                    parse(z_html:unescape(Query), QueryType, Arguments, Context);
                _ ->
                    {error, enoent}
            end;
        false ->
            {error, enoent}
    end.

%% @doc Compile a stored or unsaved query and any additional mod_search terms.
%% The `args` map contains named query-language arguments. All remaining keys
%% keep their existing meaning as additional mod_search query terms.
-spec compile(Args, Context) -> #search_sql_terms{} | #search_result{} | {error, Reason}
    when
        Args :: map(),
        Context :: z:context(),
        Reason :: term().
compile(Args, Context) when is_map(Args) ->
    case arguments(Args) of
        {ok, Arguments} ->
            case compile_query(Args, Arguments, Context) of
                {ok, #{ search_terms := #search_result{} = EmptyResult }} ->
                    EmptyResult;
                {ok, #{ search_terms := SearchTerms }} ->
                    merge_extra_terms(SearchTerms, extra_args(Args), Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
compile(_Args, _Context) ->
    {error, invalid_arguments}.

compile_query(Args, Arguments, Context) ->
    case query_arg(<<"query_id">>, Args, undefined) of
        undefined ->
            case query_arg(<<"query_text">>, Args, undefined) of
                undefined ->
                    {error, missing_query};
                Query ->
                    QueryType = query_arg(<<"query_type">>, Args, undefined),
                    parse(Query, QueryType, Arguments, Context)
            end;
        Id ->
            from_resource(Id, Arguments, Context)
    end.

arguments(Args) ->
    case query_arg(<<"args">>, Args, #{}) of
        Arguments when is_map(Arguments) -> {ok, Arguments};
        _ -> {error, invalid_arguments}
    end.

extra_args(Args) ->
    Envelope = [ <<"query_id">>, <<"query_text">>, <<"query_type">>, <<"args">> ],
    Args1 = maps:without(Envelope, Args),
    case Args1 of
        #{ <<"q">> := Terms } when is_list(Terms) ->
            Terms1 = [
                Term
                || Term <- Terms,
                   not is_envelope_term(Term, Envelope)
            ],
            Args1#{ <<"q">> => Terms1 };
        _ ->
            Args1
    end.

query_arg(Key, Args, Default) ->
    case maps:find(Key, Args) of
        {ok, Value} -> Value;
        error -> z_search:lookup_qarg_value(Key, Args, Default)
    end.

is_envelope_term(#{ <<"term">> := Term }, Envelope) ->
    lists:member(Term, Envelope);
is_envelope_term(_Term, _Envelope) ->
    false.

merge_extra_terms(SearchTerms, ExtraArgs, _Context) when map_size(ExtraArgs) =:= 0 ->
    SearchTerms;
merge_extra_terms(#search_sql_terms{} = SearchTerms, ExtraArgs, Context) ->
    case search_query:search(ExtraArgs, Context) of
        #search_sql_terms{} = ExtraTerms -> merge_terms(SearchTerms, ExtraTerms);
        #search_result{} = EmptyResult -> EmptyResult
    end.

merge_terms(
        #search_sql_terms{ terms = Terms, post_func = PostFunc },
        #search_sql_terms{ terms = ExtraTerms, post_func = undefined }) ->
    #search_sql_terms{
        terms = Terms ++ ExtraTerms,
        post_func = PostFunc
    };
merge_terms(
        #search_sql_terms{ terms = Terms, post_func = undefined },
        #search_sql_terms{ terms = ExtraTerms, post_func = PostFunc }) ->
    #search_sql_terms{
        terms = Terms ++ ExtraTerms,
        post_func = PostFunc
    };
merge_terms(#search_sql_terms{}, #search_sql_terms{}) ->
    {error, multiple_post_processing_functions}.

normalize_query_type(undefined) -> undefined;
normalize_query_type(<<>>) -> undefined;
normalize_query_type(QueryType) when is_binary(QueryType) -> QueryType;
normalize_query_type(QueryType) when is_atom(QueryType) -> atom_to_binary(QueryType, utf8);
normalize_query_type(QueryType) -> z_convert:to_binary(QueryType).

validate_result({ok, #{
        query_type := QueryType,
        query_type_label := _Label,
        parsed := _Parsed,
        search_terms := SearchTerms,
        is_live := IsLive,
        show_parsed := ShowParsed
    } = Result}, _RequestedType, _Context)
        when is_binary(QueryType),
             (is_record(SearchTerms, search_sql_terms) orelse is_record(SearchTerms, search_result)),
             is_boolean(IsLive), is_boolean(ShowParsed) ->
    {ok, Result};
validate_result({error, {query_parse, #{
        query_type := QueryType,
        query_type_label := _Label,
        is_live := IsLive,
        show_parsed := ShowParsed,
        reason := _Reason
    }}} = Error, _RequestedType, _Context)
        when (is_binary(QueryType) orelse QueryType =:= undefined),
             is_boolean(IsLive), is_boolean(ShowParsed) ->
    Error;
validate_result(undefined, RequestedType, Context) ->
    query_parse_error(RequestedType, unsupported_query_type, Context);
validate_result(_Result, RequestedType, Context) ->
    query_parse_error(RequestedType, invalid_parser_result, Context).

query_parse_error(QueryType, Reason, Context) ->
    QueryTypeLabel = case QueryType of
        undefined -> ?__("Unknown query format", Context);
        _ -> QueryType
    end,
    {error, {query_parse, #{
        query_type => QueryType,
        query_type_label => QueryTypeLabel,
        is_live => false,
        show_parsed => false,
        reason => Reason
    }}}.
