%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Analytics schemas, validation, filters and bounded rollup operations.
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

-module(z_analytics).
-moduledoc("Pure analytics validation, fixed filters and bounded rollup operations.").
-export([tiers/0, bucket/2, slot/2, registry/1, definition/2, properties/2,
         matches/2, group/2, add/4, merge/2, reduce/2, coverage/3, complete/1,
         decode/1, encode/1, dimension_keys/1, available/2]).

-include("analytics.hrl").

-spec tiers() -> [{pos_integer(), pos_integer()}].
tiers() -> [{60,360}, {900,672}, {3600,720}, {86400,1826}].
-spec bucket(Time, Resolution) -> integer() when Time :: integer(), Resolution :: pos_integer().
bucket(T, R) -> (T div R) * R.
-spec slot(Time, Resolution) -> non_neg_integer() when Time :: non_neg_integer(), Resolution :: pos_integer().
slot(T, R) -> (T div R) rem proplists:get_value(R, tiers()).
encode(T) -> term_to_binary(T).
decode(B) when is_binary(B) -> binary_to_term(B, [safe]).

-spec registry(Context) -> map() when Context :: z:context().
registry(Context) ->
    Builtin = #{name => <<"page_request">>, version => 1,
        title => <<"Public page requests">>,
        dimensions => #{
            <<"page">> => text, <<"language">> => text, <<"referrer">> => text,
            <<"has_page_id">> => [<<"true">>,<<"false">>],
            <<"country">> => text,
            <<"device">> => [<<"phone">>, <<"tablet">>, <<"desktop">>, <<"unknown">>],
            <<"traffic">> => [<<"crawler">>, <<"agent">>, <<"other">>, <<"unknown">>]}},
    Api = #{
        name => <<"api_request">>,
        version => 1,
        title => <<"HTTP API requests">>,
        dimensions => #{
            <<"method">> => [
                <<"GET">>,<<"HEAD">>,<<"POST">>,<<"PUT">>,<<"PATCH">>,
                <<"DELETE">>,<<"OPTIONS">>,<<"OTHER">>
            ],
            <<"has_page_id">> => [<<"true">>,<<"false">>],
            <<"http_status">> => http_status,
            <<"status">> => [<<"1xx">>,<<"2xx">>,<<"3xx">>,<<"4xx">>,<<"5xx">>],
            <<"language">> => text, <<"country">> => text,
            <<"traffic">> => [<<"crawler">>,<<"agent">>,<<"other">>,<<"unknown">>]
        }
    },
    Request = Api#{
        name => <<"request">>,
        title => <<"Requests by dispatch type">>,
        dimensions => (maps:get(dimensions,Api))#{
            <<"type">> => text
        }
    },
    Smtp = #{
        name => <<"smtp">>,
        version => 1,
        title => <<"SMTP (Email)">>,
        dimensions => #{
            <<"outcome">> => [<<"sent">>,<<"retry">>,<<"failed">>,<<"bounced">>]
        }
    },
    Declarations = z_notifier:foldl(#analytics_events{}, [], Context),
    lists:foldl(
        fun register_event/2,
        #{
            <<"page_request">> => Builtin,
            <<"api_request">> => Api,
            <<"request">> => Request,
            <<"smtp">> => Smtp
        },
        lists:sublist(Declarations, 64)).

register_event(#{name := Name, version := V, title := Title, dimensions := D} = Decl, Acc)
        when is_binary(Name), byte_size(Name) =< 100, is_integer(V), V > 0,
             is_binary(Title), byte_size(Title) =< 200, is_map(D), map_size(D) =< 8 ->
    Valid = binary:match(Name, <<":">>) =/= nomatch
        andalso not maps:is_key(Name, Acc)
        andalso lists:all(fun({K,T}) -> valid_key(K) andalso valid_type(T) end, maps:to_list(D)),
    case Valid of
        true -> Acc#{Name => maps:with([name,version,title,dimensions], Decl)};
        false -> Acc
    end;
register_event(_, Acc) -> Acc.

valid_key(K) -> is_binary(K) andalso byte_size(K) > 0 andalso byte_size(K) =< 40
    andalso not lists:member(K, [<<"ip">>, <<"user_id">>, <<"session_id">>, <<"user_agent">>]).
valid_type(text) -> true;
valid_type(L) when is_list(L), length(L) > 0, length(L) =< 100 ->
    lists:all(fun valid_value/1, L);
valid_type(_) -> false.
valid_value(V) -> is_binary(V) andalso byte_size(V) > 0 andalso byte_size(V) =< 255
    andalso unicode:characters_to_binary(V) =:= V.

dimension_keys(Registry) -> lists:usort(lists:append([
    maps:keys(maps:get(dimensions,D)) || D <- maps:values(Registry)])).

%% A removed module or changed schema is a gap, never a stream of zeroes.
-spec available(Collector, Registry) -> boolean() when
    Collector :: map(), Registry :: map().
available(#{event := Event, schema_version := Version}, Registry) ->
    case maps:find(Event, Registry) of
        {ok, #{version := Version}} -> true;
        _ -> false
    end.

%% JSON-facing definition. One grouping field, fixed equality/inclusion filters.
-spec definition(Definition, Registry) -> {ok, map()} | {error, invalid_definition}
    when Definition :: term(), Registry :: map().
definition(D, Registry) when is_map(D), map_size(D) =< 8 ->
    try
        true = lists:all(fun(K) -> lists:member(K, [<<"name">>,<<"description">>,<<"event">>,<<"dimension">>,<<"groups">>,<<"filters">>]) end, maps:keys(D)),
        Name = maps:get(<<"name">>, D), true = valid_value(Name),
        Description = maps:get(<<"description">>,D,<<>>),
        true = is_binary(Description) andalso byte_size(Description) =< 1000,
        true = unicode:characters_to_binary(Description) =:= Description,
        Event = maps:get(<<"event">>, D), Schema = maps:get(Event, Registry),
        Dimension = maps:get(<<"dimension">>, D, <<>>),
        Ds = maps:get(dimensions, Schema),
        true = Dimension =:= <<>> orelse maps:is_key(Dimension, Ds),
        Filters = maps:get(<<"filters">>, D, #{}),
        true = is_map(Filters) andalso map_size(Filters) =< 8,
        true = lists:all(
            fun({K, Vs}) ->
                maps:is_key(K, Ds) andalso is_list(Vs) andalso length(Vs) > 0
                    andalso length(Vs) =< 100
                    andalso lists:all(fun(V) -> value_type(V, maps:get(K,Ds))
            end, Vs)
        end, maps:to_list(Filters)),
        G = case Dimension of <<>> -> 1; _ -> maps:get(<<"groups">>, D, 32) end,
        true = is_integer(G) andalso G >= 1 andalso G =< 100,
        {ok, #{
            name => Name,
            description => Description,
            event => Event,
            schema_version => maps:get(version, Schema),
            dimension => Dimension,
            filters => maps:map(fun(_,Vs) -> lists:usort(Vs) end, Filters),
            groups => G
        }}
    catch _:_ ->
        {error, invalid_definition}
    end;
definition(_, _) ->
    {error, invalid_definition}.

-spec properties(Properties, Schema) -> {ok, map()} | {error, invalid_properties}
    when Properties :: term(), Schema :: map().
properties(P, #{dimensions := Ds}) when is_map(P), map_size(P) =< 8 ->
    case lists:all(
            fun({K,V}) -> maps:is_key(K,Ds) andalso value_type(V,maps:get(K,Ds)) end,
            maps:to_list(P))
    of
        true -> {ok, P};
        false -> {error, invalid_properties}
    end;
properties(_, _) ->
    {error, invalid_properties}.

value_type(<<H,T,U>>, http_status) -> H >= $1 andalso H =< $5
    andalso T >= $0 andalso T =< $9 andalso U >= $0 andalso U =< $9;
value_type(_, http_status) -> false;
value_type(V, text) -> valid_value(V);
value_type(V, Values) when is_list(Values) -> lists:member(V,Values).

-spec matches(Collector, Properties) -> boolean() when Collector :: map(), Properties :: map().
%% Keep automated traffic available to classification and API measurements.
%% Apply this at ingestion so existing page/device/content measurements follow
%% the same policy as newly created ones, without rewriting their history.
matches(#{event := <<"page_request">>, dimension := Dimension, filters := Filters},
        #{<<"traffic">> := Traffic} = Properties)
        when (Traffic =:= <<"crawler">> orelse Traffic =:= <<"agent">>),
             (Dimension =:= <<>> orelse Dimension =:= <<"device">> orelse Dimension =:= <<"page">>) ->
    %% An explicit traffic filter opts in to automated page/content measurements.
    maps:is_key(<<"traffic">>, Filters) andalso matches(#{filters => Filters}, Properties);
matches(#{filters := Filters}, Properties) ->
    maps:fold(
        fun(K,Vs,Match) -> Match andalso lists:member(maps:get(K,Properties,<<"unknown">>),Vs) end,
        true, Filters).

-spec group(Collector, Properties) -> binary() when Collector :: map(), Properties :: map().
group(#{dimension := <<>>}, _) -> <<"$total">>;
group(#{dimension := D}, Props) -> <<"v:", (maps:get(D,Props,<<"unknown">>))/binary>>.

%% Minute admission is stable: reserve one slot for overflow, never evict a key.
-spec add(Key, Count, Counts, Limit) -> map() when
    Key :: binary(), Count :: non_neg_integer(), Counts :: map(), Limit :: pos_integer().
add(Key, N, Counts, Limit) ->
    case maps:is_key(Key, Counts) orelse Key =:= <<"$total">> of
        true -> maps:update_with(Key, fun(V) -> V+N end, N, Counts);
        false ->
            Known = map_size(maps:remove(<<"$other">>,Counts)),
            K = case Known < Limit-1 andalso Key =/= <<"$other">> of
                true -> Key; false -> <<"$other">>
            end,
            maps:update_with(K, fun(V) -> V+N end, N, Counts)
    end.

-spec merge(Left, Right) -> map() when Left :: map(), Right :: map().
merge(A,B) ->
    maps:fold(fun(K,V,Acc) -> maps:update_with(K,fun(N)->N+V end,V,Acc) end,A,B).

-spec reduce(Counts, Limit) -> map() when Counts :: map(), Limit :: pos_integer().
reduce(Counts, Limit) ->
    case maps:is_key(<<"$total">>,Counts) of
        true -> Counts;
        false ->
            Sorted = lists:sort(fun({KA,A},{KB,B}) -> A > B orelse (A =:= B andalso KA < KB) end,
                maps:to_list(maps:remove(<<"$other">>,Counts))),
            {Keep, Rest} = lists:split(min(max(0,Limit-1),length(Sorted)),Sorted),
            Other = maps:get(<<"$other">>,Counts,0) + lists:sum([N || {_,N} <- Rest]),
            case Other of 0 -> maps:from_list(Keep); _ -> (maps:from_list(Keep))#{<<"$other">> => Other} end
    end.

%% Durable minute coverage: one bit per observed second, never fill downtime.
-spec coverage(Start, From, To) -> non_neg_integer()
    when Start :: integer(), From :: integer(), To :: integer().
coverage(Start, From, To) ->
    A = max(Start,From), B = min(Start+60,To),
    case B > A of true -> ((1 bsl (B-A))-1) bsl (A-Start); false -> 0 end.

-spec complete(Mask) -> boolean() when Mask :: non_neg_integer().
complete(Mask) -> Mask =:= (1 bsl 60)-1.
