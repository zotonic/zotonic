%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2019 Marc Worrell
%% Date: 2009-04-09
%%
%% Copyright 2009-2019 Marc Worrell
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

-module(m_edge).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get/2,
    get_triple/2,
    get_id/4,
    get_edges/2,
    insert/4,
    insert/5,
    delete/2,
    delete/4,
    delete/5,
    delete_multiple/4,
    replace/4,
    duplicate/3,
    merge/3,
    update_nth/5,
    object/4,
    subject/4,
    objects/3,
    subjects/3,
    objects/2,
    subjects/2,
    object_edge_ids/3,
    subject_edge_ids/3,
    object_edge_props/3,
    subject_edge_props/3,
    update_sequence/4,
    set_sequence/4,
    update_sequence_edge_ids/4,
    object_predicates/2,
    subject_predicates/2,
    object_predicate_ids/2,
    subject_predicate_ids/2
]).

-include_lib("zotonic.hrl").

-type insert_options() :: [ insert_option() ].

-type insert_option() :: is_insert_before
                       | no_touch
                       | {seq, integer()}
                       | {creator_id, m_rsc:resource_id()}
                       | {created, calendar:datetime()}.

-export_type([
    insert_options/0,
    insert_option/0
]).


%% @doc Fetch all object/edge ids for a subject/predicate
-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"o">>, Id, Pred | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {object_edge_ids(Id, Pred, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"o_props">>, Id, Pred | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {object_edge_props(Id, Pred, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s">>, Id, Pred | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {subject_edge_ids(Id, Pred, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s_props">>, Id, Pred | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {subject_edge_props(Id, Pred, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"edges">>, Id | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {get_edges(Id, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"id">>, SubjectId, Pred, ObjectId | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(SubjectId, Context) of
        true ->
            % m.edge.id[subject_id].predicatename[object_id] returns the
            % corresponding edge id or undefined.
            V = z_depcache:memo(
                fun() ->
                    get_id(SubjectId, Pred, ObjectId, Context)
                end,
                {get_id, SubjectId, Pred, ObjectId}, ?DAY, [SubjectId], Context),
            {ok, {V, Rest}};
        false ->
            {error, eacces}
    end;
m_get([Id], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> {ok, {get_edges(Id, Context), []}};
        false -> {error, eacces}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


%% @doc Get the complete edge with the id
get(Id, Context) ->
    z_db:assoc_row("select * from edge where id = $1", [Id], Context).

%% @doc Get the edge as a triple {subject_id, predicate, object_id}
-spec get_triple(pos_integer(), #context{}) -> {m_rsc:resource_id(), atom(), m_rsc:resource_id()}.
get_triple(Id, Context) ->
    {SubjectId, Predicate, ObjectId} = z_db:q_row("
            select e.subject_id, r.name, e.object_id
            from edge e join rsc r on e.predicate_id = r.id
            where e.id = $1", [Id], Context),
    {SubjectId, z_convert:to_atom(Predicate), ObjectId}.

%% @doc Get the edge id of a subject/pred/object combination
-spec get_id(m_rsc:resource(), m_rsc:resource(), m_rsc:resource(), #context{}) -> pos_integer() | undefined.
get_id(SubjectId, PredId, ObjectId, Context)
    when is_integer(SubjectId), is_integer(PredId), is_integer(ObjectId) ->
    z_db:q1(
        "select id from edge where subject_id = $1 and object_id = $3 and predicate_id = $2",
        [SubjectId, PredId, ObjectId],
        Context
    );
get_id(undefined, _PredId, _ObjectId, _Context) -> undefined;
get_id(_SubjectId, undefined, _ObjectId, _Context) -> undefined;
get_id(_SubjectId, _PredId, undefined, _Context) -> undefined;
get_id(Subject, Pred, ObjectId, Context) when not is_integer(Subject) ->
    get_id(m_rsc:rid(Subject, Context), Pred, ObjectId, Context);
get_id(SubjectId, Pred, ObjectId, Context) when not is_integer(Pred) ->
    case m_predicate:name_to_id(Pred, Context) of
        {ok, PredId} -> get_id(SubjectId, PredId, ObjectId, Context);
        {error, _} -> undefined
    end;
get_id(SubjectId, Pred, Object, Context) when not is_integer(Object) ->
    get_id(SubjectId, Pred, m_rsc:rid(Object, Context), Context).

%% @doc Return the full description of all edges from a subject, grouped by predicate
get_edges(SubjectId, Context) ->
    case m_rsc:rid(SubjectId, Context) of
        undefined ->
            [];
        SubjectId1 ->
            case z_depcache:get({edges, SubjectId1}, Context) of
                {ok, Edges} ->
                    Edges;
                undefined ->
                    Edges = z_db:assoc("
                        select e.id, e.subject_id, e.predicate_id, p.name, e.object_id,
                               e.seq, e.created, e.creator_id
                        from edge e join rsc p on p.id = e.predicate_id
                        where e.subject_id = $1
                        order by e.predicate_id, e.seq, e.id", [SubjectId1], Context),
                    Edges1 = z_utils:group_proplists(name, Edges),
                    Edges2 = [ {z_convert:to_atom(Pred), Es} || {Pred, Es} <- Edges1 ],
                    z_depcache:set({edges, SubjectId1}, Edges2, ?DAY, [SubjectId1], Context),
                    Edges2
            end
    end.

%% @doc Insert a new edge
-spec insert(m_rsc:resource(), m_rsc:resource(), m_rsc:resource(), z:context()) ->
    {ok, EdgeId :: pos_integer()} | {error, term()}.
insert(Subject, Pred, Object, Context) ->
    insert(Subject, Pred, Object, [], Context).

-spec insert(m_rsc:resource(), m_rsc:resource(), m_rsc:resource(), insert_options(), z:context()) ->
        {ok, EdgeId :: pos_integer()} | {error, term()}.
insert(SubjectId, PredId, ObjectId, Opts, Context)
    when is_integer(SubjectId), is_integer(PredId), is_integer(ObjectId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> insert1(SubjectId, PredId, ObjectId, Opts, Context);
        false -> {error, {unknown_predicate, PredId}}
    end;
insert(SubjectId, Pred, ObjectId, Opts, Context)
    when is_integer(SubjectId), is_integer(ObjectId) ->
    {ok, PredId} = m_predicate:name_to_id(Pred, Context),
    insert1(SubjectId, PredId, ObjectId, Opts, Context);
insert(SubjectId, Pred, Object, Opts, Context) when is_integer(SubjectId) ->
    case m_rsc:rid(Object, Context) of
        undefined -> {error, object};
        Id -> insert(SubjectId, Pred, Id, Opts, Context)
    end;
insert(Subject, Pred, Object, Opts, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined -> {error, subject};
        Id -> insert(Id, Pred, Object, Opts, Context)
    end.

insert1(SubjectId, PredId, ObjectId, Opts, Context) ->
    case z_db:q1("select id
              from edge
              where subject_id = $1
                and object_id = $2
                and predicate_id = $3",
        [SubjectId, ObjectId, PredId],
        Context)
    of
        undefined ->
            F = fun(Ctx) ->
                SeqOpt = maybe_seq_opt(Opts, SubjectId, PredId, Ctx),
                CreatedOpt = case proplists:get_value(created, Opts) of
                                 DT when is_tuple(DT) -> [{created, DT}];
                                 undefined -> []
                             end,
                EdgeProps = [
                    {subject_id, SubjectId},
                    {object_id, ObjectId},
                    {predicate_id, PredId},
                    {creator_id, case proplists:get_value(creator_id, Opts) of
                                     undefined -> z_acl:user(Ctx);
                                     CreatorId -> CreatorId
                                 end}
                    | (SeqOpt ++ CreatedOpt)
                ],
                z_db:insert(edge, EdgeProps, Ctx)
            end,
            {ok, PredName} = m_predicate:id_to_name(PredId, Context),
            case z_acl:is_allowed(insert,
                                  #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId},
                                  Context)
            of
                true ->
                    {ok, EdgeId} = z_db:transaction(F, Context),
                    z_edge_log_server:check(Context),
                    {ok, EdgeId};
                AclError ->
                    {error, {acl, AclError}}
            end;
        EdgeId ->
            % Edge exists - skip
            {ok, EdgeId}
end.

maybe_seq_opt(Opts, SubjectId, PredId, Context) ->
    case proplists:get_value(seq, Opts) of
        S when is_integer(S) ->
            [ {seq, S} ];
        _ ->
            case z_convert:to_bool( m_rsc:p_no_acl(PredId, is_insert_before, Context) )
                orelse z_convert:to_bool( proplists:get_value(is_insert_before, Opts) )
            of
                true ->
                    case z_db:q1("
                        select min(seq)
                        from edge
                        where subject_id = $1
                          and predicate_id = $2",
                        [SubjectId, PredId],
                        Context)
                    of
                        undefined -> [];
                        N -> [ {seq, N-1} ]
                    end;
                false ->
                    []
            end
    end.

%% @doc Delete an edge by Id
delete(Id, Context) ->
    {SubjectId, PredName, ObjectId} = get_triple(Id, Context),
    case z_acl:is_allowed(
        delete,
        #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
        Context
    ) of
        true ->
            F = fun(Ctx) ->
                z_db:delete(edge, Id, Ctx)
            end,

            z_db:transaction(F, Context),
            z_edge_log_server:check(Context),
            ok;
        false ->
            {error, eacces}
    end.

%% @doc Delete an edge by subject, object and predicate id
-spec delete(m_rsc:resource(), m_rsc:resource(), m_rsc:resource(), z:context()) -> ok | {error, atom()}.
delete(SubjectId, Pred, ObjectId, Context) ->
    delete(SubjectId, Pred, ObjectId, [], Context).

-spec delete(m_rsc:resource(), m_rsc:resource(), m_rsc:resource(), list(), z:context()) -> ok | {error, atom()}.
delete(SubjectId, Pred, ObjectId, _Options, Context) ->
    case to_predicate(Pred, Context) of
        {ok, PredId} ->
            {ok, PredName} = m_predicate:id_to_name(PredId, Context),
            case z_acl:is_allowed(
                delete,
                #acl_edge{ subject_id = SubjectId, predicate = PredName, object_id = ObjectId },
                Context
            ) of
                true ->
                    F = fun(Ctx) ->
                        z_db:q(
                            "delete from edge where subject_id = $1 and object_id = $2 and predicate_id = $3",
                            [SubjectId, ObjectId, PredId],
                            Ctx
                        )
                    end,

                    z_db:transaction(F, Context),
                    z_edge_log_server:check(Context),
                    ok;
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.

to_predicate(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case m_rsc:is_a(RId, predicate, Context) of
                true ->
                    {ok, RId};
                false ->
                    {error, predicate}
            end
    end.



%% @doc Delete multiple edges between the subject and the object
delete_multiple(SubjectId, Preds, ObjectId, Context) ->
    PredIds = lists:map(
        fun(Predicate) ->
            {ok, Id} = m_predicate:name_to_id(Predicate, Context),
            Id
        end,
        Preds
    ),
    PredNames = [m_predicate:id_to_name(PredId, Context) || PredId <- PredIds],
    Allowed = [z_acl:is_allowed(
        delete,
        #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
        Context
    ) || {ok, PredName} <- PredNames],
    case is_allowed(Allowed) of
        true ->
            F = fun(Ctx) ->
                z_db:q("delete
                        from edge
                        where subject_id = $1
                          and object_id = $2
                          and predicate_id in (SELECT(unnest($3::int[])))",
                    [SubjectId, ObjectId, PredIds], Ctx)
            end,

            case z_db:transaction(F, Context) of
                0 ->
                    ok;
                N when is_integer(N) ->
                    z_edge_log_server:check(Context),
                    ok;
                Error ->
                    Error
            end;
        AclError ->
            {error, {acl, AclError}}
    end.

is_allowed([]) -> true;
is_allowed([true | Rest]) -> is_allowed(Rest);
is_allowed([Error | _]) -> Error.


%% @doc Replace the objects with the new list
-spec replace(m_rsc:resource(), pos_integer() | atom(), m_rsc:resource(), #context{}) -> ok | {error, atom()}.
replace(SubjectId, PredId, NewObjects, Context) when is_integer(PredId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> replace1(SubjectId, PredId, NewObjects, Context);
        false -> {error, {unknown_predicate, PredId}}
    end;
replace(SubjectId, Pred, NewObjects, Context) ->
    {ok, PredId} = m_predicate:name_to_id(Pred, Context),
    replace1(SubjectId, PredId, NewObjects, Context).


replace1(SubjectId, PredId, NewObjects, Context) ->
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    case objects(SubjectId, PredId, Context) of
        NewObjects ->
            ok;

        CurrObjects ->
            % Check the ACL
            Allowed1 = [z_acl:is_allowed(delete,
                #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
                Context)
                || ObjectId <- CurrObjects -- NewObjects],
            Allowed2 = [z_acl:is_allowed(insert,
                #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
                Context)
                || ObjectId <- NewObjects -- CurrObjects],

            case is_allowed(Allowed1) andalso is_allowed(Allowed2) of
                true ->
                    Result = set_sequence(SubjectId, PredId, NewObjects, Context),
                    z_edge_log_server:check(Context),
                    Result;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Duplicate all edges from one id to another id. Skip all edges that give
%% ACL errors.
-spec duplicate(m_rsc:resource(), m_rsc:resource(), #context{}) ->
    ok | {error, {atom(), m_rsc:resource_id()}}.
duplicate(Id, ToId, Context) ->
    case z_acl:rsc_editable(Id, Context) andalso z_acl:rsc_editable(ToId, Context) of
        true ->
            F = fun(Ctx) ->
                FromEdges = z_db:q("
                                select predicate_id, object_id, seq
                                from edge
                                where subject_id = $1
                                order by seq, id",
                    [m_rsc:rid(Id, Context)],
                    Ctx),
                ToEdges = z_db:q(
                    "select predicate_id, object_id from edge where subject_id = $1",
                    [m_rsc:rid(ToId, Context)],
                    Ctx
                ),
                FromEdges1 = lists:filter(
                    fun({PredId, ObjectId, _Seq}) ->
                        Pair = {PredId, ObjectId},
                        not lists:member(Pair, ToEdges)
                    end,
                    FromEdges),
                FromEdges2 = lists:filter(
                    fun({PredId, ObjectId, _Seq}) ->
                        {ok, PredName} = m_predicate:id_to_name(PredId, Context),
                        z_acl:is_allowed(
                            insert,
                            #acl_edge{subject_id = ToId, predicate = PredName, object_id = ObjectId},
                            Context)
                    end,
                    FromEdges1),
                UserId = z_acl:user(Ctx),
                lists:foreach(
                    fun({PredId, ObjectId, Seq}) ->
                        z_db:insert(
                            edge,
                            [{subject_id, m_rsc:rid(ToId, Context)},
                                {predicate_id, PredId},
                                {object_id, ObjectId},
                                {seq, Seq},
                                {creator_id, UserId}],
                            Ctx)
                    end,
                    FromEdges2)
            end,
            z_db:transaction(F, Context),
            z_edge_log_server:check(Context),
            ok;
        false ->
            {error, {eacces, Id}}
    end.

%% @doc Move all edges from one id to another id, part of m_rsc:merge_delete/3
merge(WinnerId, LoserId, Context) ->
    case {z_acl:rsc_editable(WinnerId, Context), z_acl:rsc_deletable(LoserId, Context)} of
        {true, true} ->
            F = fun(Ctx) ->
                %% Edges outgoing from the looser
                LoserOutEdges = z_db:q("select predicate_id, object_id, id
                                         from edge
                                         where subject_id = $1",
                    [LoserId],
                    Ctx),
                WinnerOutEdges = z_db:q("select predicate_id, object_id
                                         from edge
                                         where subject_id = $1",
                    [WinnerId],
                    Ctx),
                LoserOutEdges1 = lists:filter(
                                fun({PredId, ObjectId, _EdgeId}) ->
                                    not lists:member({PredId, ObjectId}, WinnerOutEdges)
                                end,
                                LoserOutEdges),
                % TODO: discuss if we should enact these extra ACL checks
                % LoserOutEdges2 = lists:filter(
                %                 fun({PredId, ObjectId, _EdgeId}) ->
                %                     {ok, PredName} = m_predicate:id_to_name(PredId, Context),
                %                     z_acl:is_allowed(
                %                         insert,
                %                         #acl_edge{subject_id=WinnerId, predicate=PredName, object_id=ObjectId},
                %                         Context)
                %                 end,
                %                 LoserOutEdges1),
                lists:foreach(
                        fun({_PredId, _ObjId, EdgeId}) ->
                            z_db:equery("
                                insert into edge
                                    (subject_id, predicate_id, object_id, created, creator_id)
                                select $1, e.predicate_id, e.object_id, e.created, e.creator_id
                                from edge e
                                where e.id = $2",
                                [ WinnerId, EdgeId ],
                                Context)
                        end,
                        LoserOutEdges1),

                %% Edges incoming to the looser
                LoserInEdges = z_db:q("select predicate_id, subject_id, id
                                        from edge
                                        where object_id = $1",
                                       [LoserId],
                                       Ctx),
                WinnerInEdges = z_db:q("select predicate_id, subject_id
                                        from edge
                                        where object_id = $1",
                                       [WinnerId],
                                       Ctx),
                LoserInEdges1 = lists:filter(
                                    fun({PredId, SubjectId, _EdgeId}) ->
                                        not lists:member({PredId, SubjectId}, WinnerInEdges)
                                    end,
                                    LoserInEdges),
                lists:foreach(
                        fun({_PredId, _SubjId, EdgeId}) ->
                            z_db:equery("
                                insert into edge
                                    (subject_id, predicate_id, object_id, created, creator_id)
                                select e.subject_id, e.predicate_id, $1, e.created, e.creator_id
                                from edge e
                                where e.id = $2",
                                [ WinnerId, EdgeId ],
                                Context)
                        end,
                        LoserInEdges1),

                z_db:q("update edge set creator_id = $1 where creator_id = $2",
                       [WinnerId, LoserId],
                       Context)
            end,
            z_db:transaction(F, Context),
            z_edge_log_server:check(Context),
            ok;
        {false, _} ->
            {error, {eacces, WinnerId}};
        {_, false} ->
            {error, {eacces, WinnerId}}
    end.


%% @doc Update the nth edge of a subject.  Set a new object, keep the predicate.
%% If there are not enough edges then an error is returned. The first edge is nr 1.
-spec update_nth( m_rsc:resource_id(), m_rsc:resource(), m_rsc:resource_id(), integer(), z:context() )
    -> {ok, pos_integer()} | {error, eacces|enoent}.
update_nth(SubjectId, Predicate, Nth, ObjectId, Context) ->
    {ok, PredId} = m_predicate:name_to_id(Predicate, Context),
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    F = fun(Ctx) ->
        case z_db:q(
            "select id, object_id, seq from edge "
            "where subject_id = $1 and predicate_id = $2 "
            "order by seq,id limit 1 offset $3",
            [SubjectId, PredId, Nth - 1],
            Ctx
        ) of
            [] ->
                {error, enoent};
            [ {EdgeId, ObjectId, _SeqNr}] ->
                {ok, EdgeId};
            [ {EdgeId, OldObjectId, SeqNr} ] ->
                case z_acl:is_allowed(delete, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=OldObjectId}, Ctx) of
                    true ->
                        1 = z_db:q("delete from edge where id = $1", [EdgeId], Ctx),
                        z_db:insert(
                            edge,
                            [
                                {subject_id, SubjectId},
                                {predicate_id, PredId},
                                {object_id, ObjectId},
                                {seq, SeqNr}
                            ],
                            Ctx);
                    false ->
                        {error, eacces}
                end
        end
    end,
    case z_acl:is_allowed(
        insert,
        #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
        Context
    ) of
        true ->
            case z_db:transaction(F, Context) of
                {ok, EdgeId} ->
                    z_edge_log_server:check(Context),
                    {ok, EdgeId};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


%% @doc Return the Nth object with a certain predicate of a subject.
object(Id, Pred, N, Context) ->
    Ids = objects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch
        _:_ -> undefined
    end.

%% @doc Return the Nth subject with a certain predicate of an object.
subject(Id, Pred, N, Context) ->
    Ids = subjects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch
        _:_ -> undefined
    end.

%% @doc Return all object ids of an id with a certain predicate. The order of the ids is deterministic.
-spec objects(SubjectId::m_rsc:resource(), Predicate::m_rsc:resource(), z:context()) -> list( m_rsc:resource_id() ).
objects(_Id, undefined, _Context) ->
    [];
objects(Id, Pred, Context) when is_integer(Pred) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        SubjectId ->
            case z_depcache:get({objects, Pred, SubjectId}, Context) of
                {ok, Objects} ->
                    Objects;
                undefined ->
                    Ids = z_db:q(
                        "select object_id from edge "
                        "where subject_id = $1 and predicate_id = $2 "
                        "order by seq,id",
                        [SubjectId, Pred],
                        Context
                    ),
                    Objects = [ObjId || {ObjId} <- Ids],
                    z_depcache:set({objects, Pred, SubjectId}, Objects, ?DAY, [SubjectId], Context),
                    Objects
            end
    end;
objects(Id, Pred, Context) ->
    case m_predicate:name_to_id(Pred, Context) of
        {error, _} -> [];
        {ok, PredId} -> objects(Id, PredId, Context)
    end.


%% @doc Return all subject ids of an object id with a certain predicate.
%% The order of the ids is deterministic.
-spec subjects(ObjectId::m_rsc:resource(), Predicate::m_rsc:resource(), z:context()) -> list( m_rsc:resource_id() ).
subjects(_Id, undefined, _Context) ->
    [];
subjects(Id, Pred, Context) when is_integer(Pred) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        ObjectId ->
            case z_depcache:get({subjects, Pred, ObjectId}, Context) of
                {ok, Objects} ->
                    Objects;
                undefined ->
                    Ids = z_db:q(
                        "select subject_id from edge "
                        "where object_id = $1 and predicate_id = $2 "
                        "order by id",
                        [ObjectId, Pred],
                        Context
                    ),
                    Subjects = [SubjId || {SubjId} <- Ids],
                    z_depcache:set({subjects, Pred, ObjectId}, Subjects, ?HOUR, [ObjectId], Context),
                    Subjects
            end
    end;
subjects(Id, Pred, Context) ->
    case m_predicate:name_to_id(Pred, Context) of
        {error, _} -> [];
        {ok, PredId} -> subjects(Id, PredId, Context)
    end.


%% @doc Return all object ids of the resource
%% @spec objects(Id, Context) -> list()
objects(Id, Context) ->
    F = fun() ->
        Ids = z_db:q(
            "select object_id from edge "
            "where subject_id = $1 "
            "order by predicate_id, seq, id",
            [Id],
            Context
        ),
        [ObjId || {ObjId} <- Ids]
    end,
    z_depcache:memo(F, {objects, Id}, ?DAY, [Id], Context).

%% @doc Return all subject ids of the resource
%% @spec subjects(Id, Context) -> list()
subjects(Id, Context) ->
    F = fun() ->
        Ids = z_db:q(
            "select subject_id from edge "
            "where object_id = $1 order by predicate_id, id",
            [Id],
            Context
        ),
        [SubjId || {SubjId} <- Ids]
    end,
    z_depcache:memo(F, {subjects, Id}, ?HOUR, [Id], Context).


%% @doc Return all object ids with the edge id for a predicate/subject_id
%% @spec object_edge_ids(Id, Predicate, Context) -> list()
object_edge_ids(Id, Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, PredId} ->
            F = fun() ->
                z_db:q(
                    "select object_id, id from edge "
                    "where subject_id = $1 and predicate_id = $2 "
                    "order by seq, id",
                    [Id, PredId],
                    Context
                )
            end,
            z_depcache:memo(F, {object_edge_ids, Id, PredId}, ?DAY, [Id], Context);
        {error, _} ->
            []
    end.


%% @doc Return all subject ids with the edge id for a predicate/object_id
%% @spec subject_edge_ids(Id, Predicate, Context) -> list()
subject_edge_ids(Id, Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, PredId} ->
            F = fun() ->
                z_db:q(
                    "select subject_id, id from edge "
                    "where object_id = $1 and predicate_id = $2 "
                    "order by seq, id",
                    [Id, PredId],
                    Context
                )
            end,
            z_depcache:memo(F, {subject_edge_ids, Id, PredId}, ?DAY, [Id], Context);
        {error, _} ->
            []
    end.


%% @doc Return all object ids with edge properties
-spec object_edge_props(m_rsc:resource_id(), binary()|string()|atom()|integer(), z:context()) -> list().
object_edge_props(Id, Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, PredId} ->
            F = fun() ->
                z_db:assoc("
                        select *
                        from edge
                        where subject_id = $1
                          and predicate_id = $2
                        order by seq, id",
                    [Id, PredId],
                    Context)
            end,
            z_depcache:memo(F, {object_edge_props, Id, PredId}, ?DAY, [Id], Context);
        {error, _} ->
            []
    end.

%% @doc Return all subject ids with the edge properties
-spec subject_edge_props(m_rsc:resource_id(), binary()|string()|atom()|integer(), z:context()) -> list().
subject_edge_props(Id, Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, PredId} ->
            F = fun() ->
                z_db:assoc("
                        select *
                        from edge
                        where object_id = $1
                          and predicate_id = $2
                        order by seq, id",
                    [Id, PredId],
                    Context)
            end,
            z_depcache:memo(F, {subject_edge_props, Id, PredId}, ?DAY, [Id], Context);
        {error, _} ->
            []
    end.


%% @doc Reorder the edges so that the mentioned ids are in front, in the listed order.
%% @spec update_sequence(Id, Predicate, ObjectIds, Context) -> ok | {error, Reason}
update_sequence(Id, Pred, ObjectIds, Context) when is_integer(Id) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            {ok, PredId} = m_predicate:name_to_id(Pred, Context),
            F = fun(Ctx) ->
                All = z_db:q("
                            select object_id, id
                            from edge
                            where predicate_id = $1
                              and subject_id = $2", [PredId, Id], Ctx),

                MissingIds = lists:foldl(
                    fun({OId, _}, Acc) ->
                        case lists:member(OId, ObjectIds) of
                            true -> Acc;
                            false -> [OId | Acc]
                        end
                    end,
                    [],
                    All),

                SortedIds = ObjectIds ++ lists:reverse(MissingIds),
                SortedEdgeIds = [proplists:get_value(OId, All, -1) || OId <- SortedIds],
                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                ok
            end,

            Result = z_db:transaction(F, Context),
            z_depcache:flush(Id, Context),
            Result;
        false ->
            {error, eacces}
    end.


%% @doc Set edges order so that the specified object ids are in given order.
%% Any extra edges not specified will be deleted, and any missing edges will be inserted.
%% @spec set_sequence(Id, Predicate, ObjectIds, Context) -> ok | {error, Reason}
set_sequence(Id, Pred, ObjectIds, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            {ok, SubjectId} = m_rsc:name_to_id(Id, Context),
            {ok, PredId} = m_predicate:name_to_id(Pred, Context),
            F = fun(Ctx) ->
                All = z_db:q("
                            select object_id, id
                            from edge
                            where predicate_id = $1
                              and subject_id = $2", [PredId, SubjectId], Ctx),

                [delete(EdgeId, Context) || {ObjectId, EdgeId} <- All, not lists:member(ObjectId, ObjectIds)],
                NewEdges = [begin
                    {ok, EdgeId} = insert(Id, Pred, ObjectId, Context),
                    {ObjectId, EdgeId}
                end
                    || ObjectId <- ObjectIds,
                    not lists:member(ObjectId, All)
                ],

                AllEdges = All ++ NewEdges,
                SortedEdgeIds = [proplists:get_value(OId, AllEdges, -1) || OId <- ObjectIds],
                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                ok
            end,

            Result = z_db:transaction(F, Context),
            z_depcache:flush(Id, Context),
            Result;
        false ->
            {error, eacces}
    end.


%% @doc Update the sequence for the given edge ids.  Optionally rename the predicate on the edge.
%% @spec update_sequence_edge_ids(Id, Predicate, EdgeIds, Context) -> ok | {error, Reason}
update_sequence_edge_ids(Id, Pred, EdgeIds, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            {ok, PredId} = m_predicate:name_to_id(Pred, Context),
            F = fun(Ctx) ->
                % Figure out which edge ids need to be renamed to this predicate.
                Current = z_db:q("
                            select id
                            from edge
                            where predicate_id = $1
                              and subject_id = $2", [PredId, Id], Ctx),
                CurrentIds = [EdgeId || {EdgeId} <- Current],

                WrongPred = lists:foldl(
                    fun(EdgeId, Acc) ->
                        case lists:member(EdgeId, CurrentIds) of
                            true -> Acc;
                            false -> [EdgeId | Acc]
                        end
                    end,
                    [],
                    EdgeIds),

                %% Remove the edges where we don't have permission to remove the
                %% old predicate and insert the new predicate.
                {ok, Pred} = m_predicate:id_to_name(PredId, Ctx),
                WrongPredAllowed = lists:filter(
                    fun(EdgeId) ->
                        {Id, EdgePredName, EdgeObjectId} = get_triple(EdgeId, Ctx),
                        case z_acl:is_allowed(delete,
                            #acl_edge{subject_id = Id, predicate = EdgePredName, object_id = EdgeObjectId},
                            Ctx
                        ) of
                            true ->
                                case z_acl:is_allowed(insert,
                                    #acl_edge{subject_id = Id, predicate = Pred, object_id = EdgeObjectId},
                                    Ctx
                                ) of
                                    true -> true;
                                    _ -> false
                                end;
                            _ ->
                                false
                        end
                    end, WrongPred),

                % Update the predicates on the edges that don't have the correct predicate.
                % We have to make sure that the "wrong" edges do have the correct subject_id
                Extra = lists:foldl(
                    fun(EdgeId, Acc) ->
                        case z_db:q(
                            "update edge set predicate_id = $1 "
                            "where id = $2 and subject_id = $3",
                            [PredId, EdgeId, Id],
                            Ctx
                        ) of
                            1 -> [EdgeId | Acc];
                            0 -> Acc
                        end
                    end,
                    [],
                    WrongPredAllowed),
                All = CurrentIds ++ Extra,

                %% Extract all edge ids that are not in our sort list, they go to the end of the new sequence
                AppendToEnd = lists:foldl(
                    fun(EdgeId, Acc) ->
                        case lists:member(EdgeId, EdgeIds) of
                            true -> Acc;
                            false -> [EdgeId | Acc]
                        end
                    end,
                    [],
                    All),
                SortedEdgeIds = EdgeIds ++ lists:reverse(AppendToEnd),
                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                ok
            end,

            Result = z_db:transaction(F, Context),
            z_depcache:flush(Id, Context),
            Result;
        false ->
            {error, eacces}
    end.


%% @doc Return the list of predicates in use by edges to objects from the id
%% @spec object_predicates(Id, Context) -> List
object_predicates(Id, Context) ->
    F = fun() ->
        Ps = z_db:q(
            "select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.subject_id = $1 "
            "order by name", [Id], Context),
        [list_to_atom(binary_to_list(P)) || {P} <- Ps]
    end,
    z_depcache:memo(F, {object_preds, Id}, ?DAY, [Id], Context).

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec subject_predicates(Id, Context) -> List
subject_predicates(Id, Context) ->
    F = fun() ->
        Ps = z_db:q(
            "select distinct p.name from edge e join rsc p on e.predicate_id = p.id "
            "where e.object_id = $1 order by name",
            [Id],
            Context
        ),
        [list_to_atom(binary_to_list(P)) || {P} <- Ps]
    end,
    z_depcache:memo(F, {subject_preds, Id}, ?DAY, [Id], Context).

%% @doc Return the list of predicate ids in use by edges to objects from the id
%% @spec object_predicate_ids(Id, Context) -> List
object_predicate_ids(Id, Context) ->
    Ps = z_db:q("select distinct predicate_id from edge where subject_id = $1", [Id], Context),
    [P || {P} <- Ps].

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec subject_predicate_ids(Id, Context) -> List
subject_predicate_ids(Id, Context) ->
    Ps = z_db:q("select distinct predicate_id from edge where object_id = $1", [Id], Context),
    [P || {P} <- Ps].
