%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Apply bulk resource property updates and optional incoming and outgoing connections.
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

-module(z_admin_bulk_update).
-author("Marc Worrell <marc@worrell.nl>").
-moduledoc("
Bulk updates with optional subject-to-page and page-to-object connections. Validate
both connection selections before changing any resources. Each selected page must
be editable; edge insertion uses the normal m_edge ACL checks. Existing edges are
kept. Failed pages are counted and logged; earlier changes may already be saved.
").

-export([update/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Update selected pages, returning the number of pages with failed operations.
%% Connection controls are removed from the properties saved on each resource.
-spec update(Ids, Fields, Context) -> {ok, non_neg_integer()} | {error, connections} when
    Ids :: [m_rsc:resource()], Fields :: map(), Context :: z:context().
update(Ids, Fields, Context) ->
    Subject = connection(subject, Fields, Context),
    Object = connection(object, Fields, Context),
    case {Subject, Object} of
        {{ok, Incoming}, {ok, Outgoing}} ->
            Props = maps:filter(fun(_K, V) -> V =/= <<>> end,
                maps:without([<<"bulk_subject_id">>, <<"bulk_subject_predicate">>,
                    <<"bulk_object_id">>, <<"bulk_object_predicate">>], Fields)),
            Connections = [C || C <- [Incoming, Outgoing], C =/= undefined],
            Failed = lists:foldl(fun(Id, N) ->
                Result = try update_page(Id, Props, Connections, Context)
                    catch Class:Reason -> {error, {Class, Reason}} end,
                case Result of
                    ok -> N;
                    {error, Error} ->
                        ?LOG_WARNING(#{in => zotonic_mod_admin,
                            text => <<"Error during bulk update of resources">>,
                            id => Id, result => error, reason => Error}),
                        N + 1
                end
            end, 0, lists:usort(Ids)),
            {ok, Failed};
        _ -> {error, connections}
    end.

%% @doc Validate a complete endpoint/predicate pair, or an unused connection option.
connection(Direction, Fields, Context) ->
    Prefix = atom_to_binary(Direction),
    Id = maps:get(<<"bulk_", Prefix/binary, "_id">>, Fields, <<>>),
    Predicate = maps:get(<<"bulk_", Prefix/binary, "_predicate">>, Fields, <<>>),
    case {Id, Predicate} of
        {<<>>, <<>>} -> {ok, undefined};
        {<<>>, _} -> {error, connections};
        {_, <<>>} -> {error, connections};
        _ ->
            case {resource_id(Id, Context), resource_id(Predicate, Context)} of
                {RscId, PredId} when is_integer(RscId), is_integer(PredId) ->
                    case z_acl:rsc_visible(RscId, Context) andalso m_predicate:is_predicate(PredId, Context) of
                        true -> {ok, {Direction, RscId, PredId}};
                        false -> {error, connections}
                    end;
                _ -> {error, connections}
            end
    end.

%% @doc Resolve a submitted resource reference without accepting structured input.
resource_id(Id, Context) when is_binary(Id); is_integer(Id); is_atom(Id) ->
    m_rsc:rid(Id, Context);
resource_id(_, _) -> undefined.

%% @doc Update an editable page and then add the requested connections.
update_page(Ref, Props, Connections, Context) ->
    Id = resource_id(Ref, Context),
    case is_integer(Id) andalso z_acl:rsc_editable(Id, Context) of
        false -> {error, eacces};
        true when map_size(Props) =:= 0 -> connect(Id, Connections, Context);
        true ->
            case m_rsc:update(Id, Props, Context) of
                {ok, _} -> connect(Id, Connections, Context);
                Error -> Error
            end
    end.

%% @doc Add incoming and outgoing edges using normal edge permissions; duplicates are idempotent.
connect(_Id, [], _Context) -> ok;
connect(Id, [{Direction, Endpoint, Predicate} | Rest], Context) ->
    {Subject, Object} = case Direction of
        subject -> {Endpoint, Id};
        object -> {Id, Endpoint}
    end,
    case m_edge:insert(Subject, Predicate, Object, Context) of
        {ok, _} -> connect(Id, Rest, Context);
        Error -> Error
    end.
