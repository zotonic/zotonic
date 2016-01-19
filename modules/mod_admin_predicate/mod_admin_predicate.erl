%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-02
%% @doc Support for editing predicates in the admin module.  Also hooks into the rsc update function to
%% save the specific fields for predicates

%% Copyright 2009 Marc Worrell
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

-module(mod_admin_predicate).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin predicate support").
-mod_description("Adds support for editing predicates to the admin.").
-mod_prio(600).
-mod_depends([admin]).
-mod_provides([]).

%% interface functions
-export([
    event/2,
    observe_rsc_update/3,
    observe_rsc_update_done/2,
    observe_rsc_delete/2,
    observe_admin_menu/3,
    observe_search_query/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").



event(#submit{message={delete_move, Args}}, Context) ->
    ToPredId = z_convert:to_integer(z_context:get_q_validated("predicate_id", Context)),
    {id, PredId} = proplists:lookup(id, Args),
    case z_acl:rsc_deletable(PredId, Context) of
        true ->
            {ok, ToPredName} = m_predicate:id_to_name(ToPredId, Context),
            case z_acl:is_allowed(insert, #acl_edge{subject_id=PredId, predicate=ToPredName, object_id=PredId}, Context) of
                true ->
                    Context1 = z_context:prune_for_async(Context),
                    spawn(fun() ->
                            pred_move_and_delete(PredId, ToPredId, Context1)
                          end),
                    z_render:wire({dialog_close, []}, Context);
                false ->
                    z_render:growl(?__("Sorry, you are not allowed to insert connections with this predicate.", Context), Context)
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
    end;
event(#postback{message={delete_all, Args}}, Context) ->
    {id, PredId} = proplists:lookup(id, Args),
    IfEmpty = proplists:get_value(if_empty, Args, false),
    case not IfEmpty orelse not m_predicate:is_used(PredId, Context) of
        true ->
            case z_acl:rsc_deletable(PredId, Context)  of
                true ->
                    Context1 = z_context:prune_for_async(Context),
                    spawn(fun() ->
                            pred_delete(PredId, Context1)
                          end),
                    z_render:wire({dialog_close, []}, Context);
                false ->
                    z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
            end;
        false ->
            z_render:wire({alert, [{message, ?__("Delete is canceled, there are connections with this predicate.", Context)}]}, Context)
    end.

pred_delete(Id, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    z_db:q("delete from edge where predicate_id = $1", [Id], Context, 120000),
    _ = m_rsc:delete(Id, Context),
    z_session_page:add_script(z_render:wire({unmask, []}, Context)).

pred_move_and_delete(FromPredId, ToPredId, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    Edges = z_db:q("select a.id 
                    from edge a
                            left join edge b
                            on  a.subject_id = b.subject_id
                            and a.object_id = b.object_id
                            and b.predicate_id = $2
                    where a.predicate_id = $1
                      and b.id is null",
                   [FromPredId, ToPredId],
                   Context,
                   120000),
    pred_move(Edges, ToPredId, 0, length(Edges), Context),
    z_db:q("delete from edge where predicate_id = $1", [FromPredId], Context, 120000),
    _ = m_rsc:delete(FromPredId, Context),
    z_session_page:add_script(z_render:wire({unmask, []}, Context)).

pred_move([], _ToPredId, _Ct, _N, _Context) ->
    ok;
pred_move(EdgeIds, ToPredId, Ct, N, Context) ->
    {UpdIds, RestIds} = take(EdgeIds, 100),
    UpdIdsS = lists:flatten(
                    z_utils:combine($,, [ integer_to_list(Id) || {Id} <- UpdIds ])),
    z_db:q("update edge
            set predicate_id = $1
            where id in ("++ UpdIdsS ++")",
           [ToPredId],
           Context,
           120000),
    Ct1 = Ct + length(UpdIds),
    maybe_progress(Ct, Ct1, N, Context),
    pred_move(RestIds, ToPredId, Ct1, N, Context).

maybe_progress(_N1, _N2, 0, _Context) ->
    ok;
maybe_progress(N1, N2, Total, Context) ->
    z_pivot_rsc:pivot_delay(Context),
    PerStep = Total / 100,
    S1 = round(N1 / PerStep),
    S2 = round(N2 / PerStep),
    case S1 of
        S2 -> ok;
        _ -> z_session_page:add_script(z_render:wire({mask_progress, [{percent,S2}]}, Context))
    end.


take(L, N) ->
    take(L, N, []).

take([], _N, Acc) ->
    {Acc, []};
take(L, 0, Acc) ->
    {Acc, L};
take([Id|L], N, Acc) ->
    take(L, N-1, [Id|Acc]).

%% @doc Check if the update contains information for a predicate.  If so then update
%% the predicate information in the db and remove it from the update props.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update(#rsc_update{id=Id}, {Changed, Props}, Context) ->
    case       proplists:is_defined(predicate_subject, Props) 
        orelse proplists:is_defined(predicate_object, Props) of

        true ->
            Subjects = proplists:get_all_values(predicate_subject, Props),
            Objects  = proplists:get_all_values(predicate_object, Props),
            m_predicate:update_noflush(Id, Subjects, Objects, Context),

            Props1 = proplists:delete(predicate_subject, 
                        proplists:delete(predicate_object, Props)),
            {true, Props1};
        false ->
            {Changed, Props}
    end.

%% @doc Whenever a predicate has been updated we have to flush the predicate cache.
observe_rsc_update_done(#rsc_update_done{pre_is_a=BeforeCatList, post_is_a=CatList}, Context) ->
    case lists:member(predicate, CatList) orelse lists:member(predicate, BeforeCatList) of
        true -> m_predicate:flush(Context);
        false -> ok
    end.

%% @doc Do not allow a predicate to be removed iff there are edges with that predicate
observe_rsc_delete(#rsc_delete{id=Id, is_a=IsA}, Context) ->
    case lists:member(predicate, IsA) of
        true ->
            case m_predicate:is_used(Id, Context) of
                true -> throw({error, is_used});
                false -> ok
            end;
        false ->
            ok
    end.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_predicate,
                parent=admin_structure,
                label=?__("Predicates", Context),
                url={admin_predicate},
                visiblecheck={acl, insert, predicate}},
     #menu_item{id=admin_edges,
                parent=admin_content,
                label=?__("Page connections", Context),
                url={admin_edges}}
     |Acc].

observe_search_query({search_query, {edges, [{predicate,Predicate}]}, _OffsetLimit}, Context) 
   when Predicate =/= undefined, Predicate =/= <<>>, Predicate =/= [] ->
    PredId = m_rsc:rid(Predicate, Context),
    #search_sql{
        select="e.id, e.subject_id, e.predicate_id, e.object_id, e.creator_id, e.created",
        from="edge e join rsc s on s.id = e.subject_id join rsc o on o.id = e.object_id",
        order="e.id desc",
        where="e.predicate_id = $1",
        args=[PredId],
        tables=[{rsc,"o"}, {rsc,"s"}],
        assoc=true
    };
observe_search_query({search_query, {edges, _Args}, _OffsetLimit}, _Context) ->
    #search_sql{
        select="e.id, e.subject_id, e.predicate_id, e.object_id, e.creator_id, e.created",
        from="edge e join rsc s on s.id = e.subject_id join rsc o on o.id = e.object_id",
        order="e.id desc",
        tables=[{rsc,"o"}, {rsc,"s"}],
        assoc=true
    };
observe_search_query(_, _Context) ->
    undefined.
