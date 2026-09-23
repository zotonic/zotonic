%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Resolve coarse property privacy policies and compile scoped SQL guards.
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

-module(z_search_acl_props).

-export([policy/2, sql/4, add_source/3, guard/3, sources_sql/4, source/2]).
-export_type([policy/0]).

-include_lib("zotonic.hrl").

-type policy() :: allow | deny |
    {privacy, non_neg_integer()}.

%% @doc Resolve once per property per query; ACL providers without property privacy allow access.
%% Do not cache this result across ACL contexts or changes in module configuration.
-spec policy(Property, Context) -> Policy when
    Property :: binary(), Context :: z:context() | undefined, Policy :: policy().
policy(_Property, undefined) -> deny;
policy(Property, Context) when is_binary(Property) ->
    case z_acl:is_admin(Context) of
        true -> allow;
        false ->
            case z_notifier:first(#acl_query_prop{property = Property}, Context) of
                allow -> allow;
                undefined -> allow;
                {privacy, MaxLevel} = Policy when is_integer(MaxLevel), MaxLevel >= 0 ->
                    Policy;
                _ -> deny
            end
    end.

%% @doc Return a parenthesized guard and appended SQL arguments for one resource alias.
%% Call in the scope where the property is read, before aggregation or pagination.
%% OPTIONAL/UNION/subquery callers must keep the guard inside that branch. This does
%% not replace resource visibility checks. Properties must be canonical source names,
%% including dependencies of pivots, facets and expressions, never SQL fragments.
%% Empty SQL means unrestricted. This API alone does not inspect or protect queries.
-spec sql(Alias, Properties, Args, Context) -> {Sql, NewArgs} when
    Alias :: binary() | string(), Properties :: [binary()], Args :: list(),
    Context :: z:context() | undefined, Sql :: binary(), NewArgs :: list().
sql(Alias, Properties, Args, Context) ->
    Policies = lists:usort([policy(P, Context) || P <- lists:usort(Properties)]),
    case lists:member(deny, Policies) of
        true -> {<<"false">>, Args};
        false ->
            %% Quote the identifier even though aliases are compiler-owned.
            Quoted = [<<"\"">>, binary:replace(z_convert:to_binary(Alias), <<"\"">>, <<"\"\"">>, [global]), <<"\"">>],
            {Conditions, Args1} = lists:mapfoldl(fun
                (Policy, Acc) -> privacy_sql(Quoted, Policy, Acc, Context)
            end, Args, lists:delete(allow, Policies)),
            {iolist_to_binary(lists:join(<<" AND ">>, Conditions)), Args1}
    end.

privacy_sql(Alias, {privacy, MaxLevel}, Args, _Context) ->
    {Allowed, Args1} = parameter(MaxLevel, Args),
    %% The lower bound excludes resources whose privacy has not been migrated.
    {[<<"(">>, Alias, <<".privacy BETWEEN 0 AND ">>, Allowed, <<"::integer)">>], Args1}.

parameter(Value, Args) ->
    parameter(Value, Args, Args, 1).

parameter(Value, [Value | _], Args, Index) ->
    {[<<"$">>, integer_to_binary(Index)], Args};
parameter(Value, [_ | Rest], Args, Index) ->
    parameter(Value, Rest, Args, Index + 1);
parameter(Value, [], Args, Index) ->
    {[<<"$">>, integer_to_binary(Index)], Args ++ [Value]}.

%% @doc Attach provenance at the point a compiler resolves a property expression.
-spec add_source(#search_sql_term{}, binary(), term()) -> #search_sql_term{}.
add_source(#search_sql_term{property_sources = Sources} = Term, Alias, Source) ->
    Term#search_sql_term{property_sources = lists:usort([{Alias, Source} | Sources])}.

%% @doc Emit guards before combining boolean branches; clear consumed metadata.
-spec guard(#search_sql_term{}, list(), z:context() | undefined) -> {#search_sql_term{}, list()}.
guard(#search_sql_term{property_sources = []} = Term, Args, _Context) ->
    {Term, Args};
guard(#search_sql_term{property_sources = Sources, where = Where} = Term, Args, Context) ->
    Aliases = lists:usort([Alias || {Alias, _} <- Sources]),
    {Guards, Args1} = lists:mapfoldl(fun(Alias, Acc) ->
        sources_sql(Alias, [S || {A, S} <- Sources, A =:= Alias], Acc, Context)
    end, Args, Aliases),
    Parts = [G || G <- Guards, G =/= <<>>] ++ case Where of
        [] -> [];
        <<>> -> [];
        _ -> [[<<"(">>, Where, <<")">>]]
    end,
    {Term#search_sql_term{property_sources = [], where = lists:join(<<" AND ">>, Parts)}, Args1}.

%% @doc Compile source dependencies for a single resource alias.
-spec sources_sql(binary(), [term()], list(), z:context() | undefined) -> {binary(), list()}.
sources_sql(Alias, Sources, Args, Context) ->
    case Context =/= undefined andalso z_acl:is_admin(Context) of
        true -> {<<>>, Args};
        false ->
            Resolved = [source(S, Context) || S <- lists:usort(Sources)],
            case lists:member(deny, Resolved) of
                true -> {<<"false">>, Args};
                false -> sql(Alias, lists:usort(lists:append([Ps || {ok, Ps} <- Resolved])), Args, Context)
            end
    end.

%% @doc Resolve trusted provenance declarations; opaque sources fail closed.
-spec source(term(), z:context() | undefined) -> {ok, [binary()]} | deny.
source(_Source, undefined) -> deny;
source(Source, Context) ->
    case z_notifier:first(#acl_query_source{source = Source}, Context) of
        undefined -> builtin_source(Source);
        {ok, Properties} when is_list(Properties) ->
            case lists:all(fun is_binary/1, Properties) of
                true -> {ok, Properties};
                false -> deny
            end;
        _ -> deny
    end.

builtin_source({column, <<"search_facet">>, _}) -> {ok, []};
builtin_source({column, <<"pivot_", _/binary>>, _}) -> {ok, []};
builtin_source({column, <<"rsc">>, <<"pivot_tsv">>}) -> {ok, []};
builtin_source({column, <<"rsc">>, <<"pivot_rtsv">>}) -> {ok, []};
builtin_source({jsonb, <<"search_facet">>, _, _}) -> {ok, []};
builtin_source({jsonb, <<"pivot_", _/binary>>, _, _}) -> {ok, []};
builtin_source({jsonb, <<"rsc">>, <<"props_json">>, Property}) when is_binary(Property), Property =/= <<>> ->
    {ok, [Property]};
builtin_source({jsonb, <<"rsc">>, <<"props_json">>, [Property | _]}) when is_binary(Property) ->
    {ok, [Property]};
builtin_source({column, <<"rsc">>, <<"props">>}) -> deny;
builtin_source({column, <<"rsc">>, <<"props_json">>}) -> deny;
builtin_source({column, <<"rsc">>, <<"pivot_", Pivot/binary>> = Column}) ->
    %% These are the actual rsc schema columns, not the pivot template block names.
    %% Keep the physical property too: ACL providers may protect it explicitly.
    Properties = #{
        <<"category_nr">> => [<<"category_id">>],
        <<"page_path">> => [<<"page_path">>],
        <<"title">> => [<<"title">>],
        <<"first_name">> => [<<"name_first">>],
        <<"surname">> => [<<"name_surname">>],
        <<"gender">> => [<<"gender">>],
        <<"street">> => [<<"address_street_1">>, <<"address_street_2">>],
        <<"city">> => [<<"address_city">>],
        <<"postcode">> => [<<"address_postcode">>],
        <<"state">> => [<<"address_state">>],
        <<"country">> => [<<"address_country">>],
        <<"date_start">> => [<<"date_start">>],
        <<"date_end">> => [<<"date_end">>],
        <<"date_start_month_day">> => [<<"date_start">>],
        <<"date_end_month_day">> => [<<"date_end">>],
        <<"location_lat">> => [<<"location_lat">>],
        <<"location_lng">> => [<<"location_lng">>],
        <<"geocode">> => [<<"location_lat">>, <<"location_lng">>],
        <<"geocode_qhash">> => [<<"location_lat">>, <<"location_lng">>]
    },
    case maps:find(Pivot, Properties) of
        {ok, Sources} -> {ok, [Column | Sources]};
        error -> deny
    end;
builtin_source({column, <<"rsc">>, Property}) when is_binary(Property) -> {ok, [Property]};
builtin_source(_) -> deny.
