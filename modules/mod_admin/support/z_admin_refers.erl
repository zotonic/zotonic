%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Maximonster Interactive Things BV
%% @doc Ensure that alle embedded ids in a resource are connected using
%% a 'refers' edge.
%% @end

%% Copyright 2023 Maximonster Interactive Things BV
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

-module(z_admin_refers).

-export([
    ensure_refers/2,
    insert_ensure_refers_all_task/1,
    task_ensure_refers_all/2
]).

-include_lib("zotonic.hrl").


%% @doc Insert a task to check all resource for embedded resource references and
%% add a refers edge for all of those references.
-spec insert_ensure_refers_all_task( Context ) -> ok when
    Context :: z:context().
insert_ensure_refers_all_task(Context) ->
    {ok, _} = z_pivot_rsc:insert_task(?MODULE, task_ensure_refers_all, <<>>, [1], Context),
    ok.


%% @doc Check all resource for embedded resource references and add a refers
%% edge for all of them.
-spec task_ensure_refers_all(FromId, Context) -> ok when
    FromId :: pos_integer(),
    Context :: z:context().
task_ensure_refers_all(FromId, Context) ->
    case z_db:q("
        select id from rsc
        where id > $1
        order by id asc
        limit 1000",
        [ FromId ],
        Context)
    of
        [] ->
            ok;
        Rs ->
            Ids = [ Id || {Id} <- Rs ],
            ContextSudo = z_acl:sudo(Context),
            lists:foreach(
                fun(Id) ->
                    ensure_refers(Id, ContextSudo)
                end, Ids),
            {delay, 0, [ lists:max(Ids) ]}
    end.


%% @doc Set the 'refers' edges to keep track which resources are used where.
%% This is needed for the automatic cleanup of 'dependent' resources.
-spec ensure_refers(Id, Context) -> ok when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
ensure_refers(Id, Context) ->
    % Keep an administration of what resources are referred from within the pivoted resource
    RscCols = z_db:column_names(rsc, Context),
    EmbeddedIds = find_ids(m_rsc:get(Id, Context), RscCols, Context),
    EmbeddedIds1 = lists:filter(fun is_integer/1, EmbeddedIds),
    Objects = m_edge:objects(Id, refers, Context),
    New = EmbeddedIds1 -- Objects,
    Old = Objects -- EmbeddedIds1,
    New1 = lists:filter(fun(ObjId) -> m_rsc:exists(ObjId, Context) end, New),
    [ m_edge:insert(Id, refers, ObjId, [no_touch], Context) || ObjId <- New1 ],
    [ m_edge:delete(Id, refers, ObjId, [no_touch], Context) || ObjId <- Old ],
    ok.

find_ids(undefined, _RscCols, _Context) ->
    [];
find_ids(Rsc, RscCols, Context) when is_list(Rsc) ->
    Ids = lists:foldl(
        fun
            ({K, V}, Acc) ->
                case lists:member(K, RscCols) of
                    true ->
                        % Table level references, ignore for programmatic
                        % reference tracking.
                        Acc;
                    false ->
                        case ids(K, V, Context) of
                            [] -> Acc;
                            VIds -> [ VIds | Acc ]
                        end
                end;
            (_, Acc) ->
                Acc
        end,
        [],
        Rsc),
    lists:usort(lists:flatten(Ids));
find_ids(Rsc, RscCols, Context) when is_map(Rsc) ->
    Ids = maps:fold(
        fun(K, V, Acc) ->
            case lists:member(K, RscCols) of
                true ->
                    % Table level references, ignore for programmatic
                    % reference tracking.
                    Acc;
                false ->
                    case ids(K, V, Context) of
                        [] -> Acc;
                        VIds -> [ VIds | Acc ]
                    end
            end
        end,
        [],
        Rsc),
    lists:usort(lists:flatten(Ids)).

ids(managed_props, _, _Context) ->
    [];
ids(K, V, Context) when is_integer(V); is_binary(V) ->
    case is_rsc_prop(K) of
        true ->
            [ m_rsc:rid(V, Context) ];
        false when is_binary(V) ->
            case is_html_prop(K) of
                true ->
                    embedded_media(V);
                false ->
                    []
            end;
        false ->
            []
    end;
ids(K, {trans, _} = V, _Context) ->
    case is_html_prop(K) of
        true ->
            embedded_media(V);
        false ->
            []
    end;
ids(_K, [ V | _ ] = L, Context) when is_map(V) ->
    lists:flatmap(
        fun(B) ->
            find_ids(B, [], Context)
        end,
        L);
ids(_K, V, Context) when is_map(V) ->
    Ids = maps:fold(
        fun
            (K1, V1, Acc) when is_binary(K1) ->
                [ ids(K1, V1, Context) | Acc ];
            (_, _, Acc) ->
                Acc
        end,
        [],
        V),
    lists:flatten(Ids);
ids(_, _, _Context) ->
    [].

is_rsc_prop(id) -> false;
is_rsc_prop(creator_id) -> false;
is_rsc_prop(modifier_id) -> false;
is_rsc_prop(category_id) -> false;
is_rsc_prop(content_group_id) -> false;
is_rsc_prop(rsc_id) -> true;
is_rsc_prop(rsc_id2) -> true;
is_rsc_prop(P0) ->
    P = z_convert:to_binary(P0),
    case binary:longest_common_suffix([ P, <<"_id">> ]) of
        3 -> true;
        _ ->
            case binary:longest_common_suffix([ P, <<"_id2">> ]) of
                4 -> true;
                _ -> false
            end
    end.

is_html_prop(body) -> true;
is_html_prop(body_extra) -> true;
is_html_prop(P0) ->
    P = z_convert:to_binary(P0),
    case binary:longest_common_suffix([ P, <<"_html">> ]) of
        5 -> true;
        _ -> false
    end.

embedded_media(undefined) ->
    [];
embedded_media(<<>>) ->
    [];
embedded_media(Input) when is_binary(Input) ->
    case re:run(Input, "\\<\\!-- z-media ([0-9]+) ", [global, {capture, all_but_first, binary}]) of
        nomatch ->
            [];
        {match, L} ->
            [ z_convert:to_integer(I) || [I] <- L ]
    end;
embedded_media({trans, Tr}) ->
    lists:flatmap(
        fun({_, B}) ->
            embedded_media(B)
        end,
        Tr);
embedded_media(_) ->
    [].

