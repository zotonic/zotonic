%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-09
%%
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

-module(m_edge).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    get/2,
    get_triple/2,
    get_id/4,
    get_edges/2,
    insert/4,
    delete/2,
    delete/4,
    delete_multiple/4,
    replace/4,
    duplicate/3,
    update_nth/5,
    object/4,
    subject/4,
    objects/3,
    subjects/3,
    objects/2,
    subjects/2,
    object_edge_ids/3,
    subject_edge_ids/3,
    update_sequence/4,
    set_sequence/4,
    update_sequence_edge_ids/4,
    object_predicates/2,
    subject_predicates/2,
    object_predicate_ids/2,
    subject_predicate_ids/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch all object/edge ids for a subject/predicate
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(o, #m{value=undefined}, _Context) ->
    fun(Id, _IdContext) ->
        fun(Pred, PredContext) ->
            object_edge_ids(Id, Pred, PredContext)
        end
    end;

m_find_value(s, #m{value=undefined}, _Context) ->
    fun(Id, _IdContext) ->
        fun(Pred, PredContext) ->
            subject_edge_ids(Id, Pred, PredContext)
        end
    end;

m_find_value(_Key, #m{}, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{}, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, _Context) ->
    undefined.


%% @doc Get the complete edge with the id
get(Id, Context) ->
    z_db:assoc_row("select * from edge where id = $1", [Id], Context).

%% @doc Get the edge as a triple {subject_id, predicate, object_id}
get_triple(Id, Context) ->
    {SubjectId, Predicate, ObjectId} = z_db:q_row("
            select e.subject_id, r.name, e.object_id 
            from edge e join rsc r on e.predicate_id = r.id 
            where e.id = $1", [Id], Context),
    {SubjectId, z_convert:to_atom(Predicate), ObjectId}.

%% @doc Get the edge id of a subject/pred/object combination
get_id(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    z_db:q1("select id from edge where subject_id = $1 and object_id = $3 and predicate_id = $2", [SubjectId, PredId, ObjectId], Context).

%% @doc Return the full description of all edges from a subject, grouped by predicate
get_edges(SubjectId, Context) ->
    case z_depcache:get({edges, SubjectId}, Context) of
        {ok, Edges} -> 
            Edges;
        undefined ->
            Edges = z_db:assoc("
                select e.id, e.subject_id, e.predicate_id, p.name, e.object_id, e.seq 
                from edge e join rsc p on p.id = e.predicate_id 
                where e.subject_id = $1 
                order by e.predicate_id, e.seq, e.id", [SubjectId], Context),
            Edges1 = z_utils:group_proplists(name, Edges),
            z_depcache:set({edges, SubjectId}, Edges1, ?DAY, [SubjectId], Context),
            Edges1
    end.

%% Insert a new edge
insert(SubjectId, PredId, ObjectId, Context) when is_integer(PredId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> insert1(SubjectId, PredId, ObjectId, Context);
        false -> throw({error, {unknown_predicate, PredId}})
    end;
insert(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    insert1(SubjectId, PredId, ObjectId, Context).
    
    insert1(SubjectId, PredId, ObjectId, Context) ->
        case z_db:q1("select id from edge where subject_id = $1 and object_id = $2 and predicate_id = $3", [SubjectId, ObjectId, PredId], Context) of
            undefined ->
                F = fun(Ctx) ->
                    m_rsc:touch(SubjectId, Ctx),
                    z_db:insert(edge, [{subject_id, SubjectId}, {object_id, ObjectId}, {predicate_id, PredId}], Ctx)
                end,
                
                {ok, PredName} = m_predicate:id_to_name(PredId, Context),
                case z_acl:is_allowed(insert, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context) of
                    true ->
                        {ok, EdgeId} = z_db:transaction(F, Context),
                        z_depcache:flush(SubjectId, Context),
                        z_depcache:flush(ObjectId, Context),
                        z_notifier:notify(#edge_insert{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context),
                        {ok, EdgeId};
                    AclError ->
                        {error, {acl, AclError}}
                end;
            EdgeId ->
                % Edge exists - skip
                {ok, EdgeId}
        end.


%% @doc Delete an edge by Id
delete(Id, Context) ->
    {SubjectId, PredName, ObjectId} = get_triple(Id, Context),
    case z_acl:is_allowed(delete, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context) of
        true ->
            F = fun(Ctx) ->
                m_rsc:touch(SubjectId, Ctx),
                z_db:delete(edge, Id, Ctx)
            end,
            
            z_db:transaction(F, Context),
            z_depcache:flush(SubjectId, Context),
            z_depcache:flush(ObjectId, Context),
            z_notifier:notify(#edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context),
            ok;
        AclError -> 
            {error, {acl, AclError}}
    end.

%% @doc Delete an edge by subject, object and predicate id
delete(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    case z_acl:is_allowed(delete, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context) of
        true ->
            F = fun(Ctx) ->
                m_rsc:touch(SubjectId, Ctx),
                z_db:q("delete from edge where subject_id = $1 and object_id = $2 and predicate_id = $3",  [SubjectId, ObjectId, PredId], Ctx)
            end,

            z_db:transaction(F, Context),
            z_depcache:flush(SubjectId, Context),
            z_depcache:flush(ObjectId, Context),
            z_notifier:notify(#edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context),
            ok;
        AclError ->
            {error, {acl, AclError}}
    end.


%% @doc Delete multiple edges between the subject and the object
delete_multiple(SubjectId, Preds, ObjectId, Context) ->
    PredIds = [ m_predicate:name_to_id_check(Pred, Context) || Pred <- Preds ],
    PredNames = [ m_predicate:id_to_name(PredId, Context) || PredId <- PredIds ],
    Allowed = [ z_acl:is_allowed(delete, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context)
                || {ok, PredName} <- PredNames ],
    case is_allowed(Allowed) of
        true ->
            F = fun(Ctx) ->
                m_rsc:touch(SubjectId, Ctx),
                z_db:q("delete from edge where subject_id = $1 and object_id = $2 and predicate_id in ("
                        ++ string:join(lists:map(fun erlang:integer_to_list/1, PredIds), ",")
                        ++ ")",
                        [SubjectId, ObjectId], Ctx)
            end,

            case z_db:transaction(F, Context) of
                0 -> 
                    ok;
                N when is_integer(N) -> 
                    z_depcache:flush(SubjectId, Context),
                    z_depcache:flush(ObjectId, Context),
                    [
                        z_notifier:notify(#edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context)
                        || PredName <- PredNames
                    ],
                    ok;
                Error -> 
                    Error
            end;
        AclError ->
            {error, {acl, AclError}}
    end.
        
    is_allowed([]) -> true;
    is_allowed([true|Rest]) -> is_allowed(Rest);
    is_allowed([Error|_]) -> Error. 



%% @doc Replace the objects with the new list
replace(SubjectId, PredId, NewObjects, Context) when is_integer(PredId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> replace1(SubjectId, PredId, NewObjects, Context);
        false -> throw({error, {unknown_predicate, PredId}})
    end;
replace(SubjectId, Pred, NewObjects, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    replace1(SubjectId, PredId, NewObjects, Context).


    replace1(SubjectId, PredId, NewObjects, Context) ->
        {ok, PredName} = m_predicate:id_to_name(PredId, Context),
        case objects(SubjectId, PredName, Context) of
            NewObjects -> 
                ok;

            CurrObjects ->
                % Check the ACL
                Allowed1 = [ z_acl:is_allowed(delete, 
                                             #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId},
                                             Context)
                            || ObjectId <- CurrObjects ],
                Allowed2 = [ z_acl:is_allowed(insert, 
                                             #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId},
                                             Context)
                            || ObjectId <- NewObjects ],

                case is_allowed(Allowed1) andalso is_allowed(Allowed2) of
                    true ->
                        Values = string:join([
                            lists:flatten([
                                $(, integer_to_list(SubjectId),
                                $,, integer_to_list(PredId),
                                $,, integer_to_list(Id),
                                $,, z_convert:to_list(z_acl:user(Context)),
                                ",now())"
                            ]) || Id <- NewObjects
                        ], ","),
                        F = fun(Ctx) ->
                            z_db:q("delete from edge where subject_id = $1 and predicate_id = $2", 
                                    [SubjectId, PredId], Ctx),
                            z_db:q("insert into edge (subject_id, predicate_id, object_id, creator_id, created)
                                    values "++Values, Ctx),
                            m_rsc:touch(SubjectId, Ctx)
                        end,
            
                        % Sync all caches, notify edge delete/insert listeners
                        z_db:transaction(F, Context),
                        z_db:flush(Context),
                        [
                            case lists:member(ObjId, NewObjects) of
                                true -> nop;
                                false -> z_notifier:notify({edge_delete, SubjectId, PredName, ObjId}, Context)
                            end
                            || ObjId <- CurrObjects
                        ],
                        [
                            case lists:member(ObjId, CurrObjects) of
                                true -> nop;
                                false -> z_notifier:notify({edge_insert, SubjectId, PredName, ObjId}, Context)
                            end
                            || ObjId <- NewObjects
                        ],
                        ok;
                    AclError ->
                        {error, {acl, AclError}}
                end
        end.


%% @doc Duplicate all edges from one id to another id.  Skip all edges that give ACL errors.
duplicate(Id, ToId, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            F = fun(Ctx) ->
                m_rsc:touch(ToId, Ctx),
                Edges = z_db:q("select predicate_id, object_id, seq from edge where subject_id = $1", [Id], Ctx),
                UserId = z_acl:user(Ctx),
                [
                    catch z_db:insert(edge, [{subject_id, ToId}, {predicate_id, PredId}, {object_id, ObjId}, {seq, Seq}, {creator_id, UserId}], Ctx)
                    || {PredId, ObjId, Seq} <- Edges
                ]
            end,
            z_db:transaction(F, Context),
            z_depcache:flush(ToId, Context),
            ok;
        false ->
            {error, {eacces, Id}}
    end.
    

%% @doc Update the nth edge of a subject.  Set a new object, keep the predicate.
%% When there are not enough edges then an error is returned. The first edge is nr 1.
%% @spec update_nth(int(), Predicate, int(), ObjectId, Context) -> {ok, EdgeId} | {error, Reason}
update_nth(SubjectId, Predicate, Nth, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Predicate, Context),
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    F = fun(Ctx) ->
        case z_db:q("select id, object_id from edge where subject_id = $1 and predicate_id = $2 order by seq,id limit 1 offset $3", 
                    [SubjectId, PredId, Nth-1], 
                    Ctx) of
            [] -> 
                {error, enoent};
            [{EdgeId,OldObjectId}] ->
                case z_acl:is_allowed(delete, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=OldObjectId}, Ctx) of
                    true ->
                        1 = z_db:q("update edge set object_id = $1, creator_id = $3, created = now() where id = $2", 
                                   [ObjectId,EdgeId,z_acl:user(Ctx)], 
                                   Ctx),
                        m_rsc:touch(SubjectId, Ctx),
                        {ok, EdgeId};
                    false ->
                        {error, eacces}
                end
        end
    end,
    case z_acl:is_allowed(insert, #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId}, Context) of
        true -> 
            case z_db:transaction(F, Context) of
                {ok,EdgeId} ->
                    z_depcache:flush(SubjectId, Context),
                    z_notifier:notify({edge_insert, SubjectId, PredName, ObjectId}, Context),
                    {ok, EdgeId};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


%% @doc Return the Nth object with a certaing predicate of a subject.
object(Id, Pred, N, Context) ->
    Ids = objects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch 
        _:_ -> undefined
    end.

%% @doc Return the Nth subject with a certaing predicate of an object.
subject(Id, Pred, N, Context) ->
    Ids = subjects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch 
        _:_ -> undefined
    end.

%% @doc Return all object ids of an id with a certain predicate.  The order of the ids is deterministic.
%% @spec objects(Id, Pred, Context) -> List
objects(_Id, undefined, _Context) ->
    [];
objects(Id, Pred, Context) when is_integer(Pred) ->
    case z_depcache:get({objects, Pred, Id}, Context) of
        {ok, Objects} ->
            Objects;
        undefined ->
            Ids = z_db:q("select object_id from edge where subject_id = $1 and predicate_id = $2 order by seq,id", [Id, Pred], Context),
            Objects = [ ObjId || {ObjId} <- Ids ],
            z_depcache:set({objects, Pred, Id}, Objects, ?DAY, [Id], Context),
            Objects
    end;
objects(Id, Pred, Context) ->
    case m_predicate:name_to_id(Pred, Context) of
        {error, _} -> [];
        {ok, PredId} -> objects(Id, PredId, Context)
    end.


%% @doc Return all subject ids of an object id with a certain predicate.   The order of the ids is deterministic.
%% @spec subjects(Id, Pred, Context) -> List
subjects(_Id, undefined, _Context) ->
    [];
subjects(Id, Pred, Context) when is_integer(Pred) ->
    case z_depcache:get({subjects, Pred, Id}, Context) of
        {ok, Objects} ->
            Objects;
        undefined ->
            Ids = z_db:q("select subject_id from edge where object_id = $1 and predicate_id = $2 order by id", [Id, Pred], Context),
            Subjects = [ SubjId || {SubjId} <- Ids ],
            z_depcache:set({subjects, Pred, Id}, Subjects, ?HOUR, [Id], Context),
            Subjects
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
        Ids = z_db:q("select object_id from edge where subject_id = $1 order by predicate_id, seq, id", [Id], Context),
        [ ObjId || {ObjId} <- Ids]
    end,
    z_depcache:memo(F, {objects, Id}, ?DAY, [Id], Context).

%% @doc Return all subject ids of the resource
%% @spec subjects(Id, Context) -> list()
subjects(Id, Context) ->
    F = fun() ->
        Ids = z_db:q("select subject_id from edge where object_id = $1 order by predicate_id, id", [Id], Context),
        [ SubjId || {SubjId} <- Ids]
    end,
    z_depcache:memo(F, {subjects, Id}, ?HOUR, [Id], Context).


%% @doc Return all object ids with the edge id for a predicate/subject_id
%% @spec object_edge_ids(Id, Predicate, Context) -> list()
object_edge_ids(Id, Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, PredId} ->
            F = fun() ->
                z_db:q("select object_id, id from edge where subject_id = $1 and predicate_id = $2 order by seq, id", [Id, PredId], Context)
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
                z_db:q("select subject_id, id from edge where object_id = $1 and predicate_id = $2 order by seq, id", [Id, PredId], Context)
            end,
            z_depcache:memo(F, {subject_edge_ids, Id, PredId}, ?DAY, [Id], Context);
        {error, _} ->
            []
    end.


%% @doc Reorder the edges so that the mentioned ids are in front, in the listed order.
%% @spec update_sequence(Id, Predicate, ObjectIds, Context) -> ok | {error, Reason}
update_sequence(Id, Pred, ObjectIds, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            PredId = m_predicate:name_to_id_check(Pred, Context),
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
                SortedEdgeIds = [ proplists:get_value(OId, All, -1) || OId <- SortedIds ],
                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                m_rsc:touch(Id, Ctx),
                ok
            end,
            
            Result = z_db:transaction(F, Context),
            z_depcache:flush(Id, Context),
            Result;
        false ->
            {error, eacces}
    end.


%% @doc Set edges order so that the specified object ids are in given order.
%% Any extra edges not speicified will be deleted, and any missing edges will be inserted.
%% @spec set_sequence(Id, Predicate, ObjectIds, Context) -> ok | {error, Reason}
set_sequence(Id, Pred, ObjectIds, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            PredId = m_predicate:name_to_id_check(Pred, Context),
            F = fun(Ctx) ->
                        All = z_db:q("
                            select object_id, id
                            from edge
                            where predicate_id = $1
                              and subject_id = $2", [PredId, Id], Ctx),

                        [ delete(EdgeId, Context) || {ObjectId, EdgeId} <- All, not lists:member(ObjectId, ObjectIds) ],
                        NewEdges = [ begin
                                         {ok, EdgeId} = insert(Id, Pred, ObjectId, Context),
                                         {ObjectId, EdgeId}
                                     end
                                     || ObjectId <- ObjectIds,
                                        not lists:member(ObjectId, All)
                                   ],

                        AllEdges = All ++ NewEdges,
                        SortedEdgeIds = [ proplists:get_value(OId, AllEdges, -1) || OId <- ObjectIds ],
                        z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                        m_rsc:touch(Id, Ctx),
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
            PredId = m_predicate:name_to_id_check(Pred, Context),
            F = fun(Ctx) ->
                % Figure out which edge ids need to be renamed to this predicate.
                Current = z_db:q("
                            select id 
                            from edge 
                            where predicate_id = $1
                              and subject_id = $2", [PredId, Id], Ctx),
                CurrentIds = [ EdgeId || {EdgeId} <- Current ],

                WrongPred = lists:foldl(
                            fun(EdgeId, Acc) ->
                                case lists:member(EdgeId, CurrentIds) of
                                    true -> Acc;
                                    false -> [EdgeId | Acc]
                                end
                            end,
                            [],
                            EdgeIds),

                %% Remove the edges where we don't have permission to remove the old predicate and insert the new predicate.
                {ok, PredName} = m_predicate:id_to_name(PredId, Ctx),
                WrongPredAllowed = lists:filter(
                                        fun (EdgeId) -> 
                                            {Id, EdgePredName, EdgeObjectId} = get_triple(EdgeId, Ctx),
                                            case z_acl:is_allowed(delete, 
                                                                  #acl_edge{subject_id=Id, predicate=EdgePredName, object_id=EdgeObjectId}, 
                                                                  Ctx) of
                                                true ->
                                                    case z_acl:is_allowed(insert, 
                                                                          #acl_edge{subject_id=Id, predicate=PredName, object_id=EdgeObjectId},
                                                                          Ctx) of
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
                                    case z_db:q("update edge set predicate_id = $1 where id = $2 and subject_id = $3", [PredId, EdgeId, Id], Ctx) of
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
                                        false -> [ EdgeId | Acc]
                                    end
                                end,
                                [],
                                All),
                SortedEdgeIds = EdgeIds ++ lists:reverse(AppendToEnd),
                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                m_rsc:touch(Id, Ctx),
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
        Ps = z_db:q("select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.subject_id = $1 order by name", [Id], Context),
        [ list_to_atom(binary_to_list(P)) || {P} <- Ps ]
    end,
    z_depcache:memo(F, {object_preds, Id}, ?DAY, [Id], Context).

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec subject_predicates(Id, Context) -> List
subject_predicates(Id, Context) ->
    F = fun() ->
        Ps = z_db:q("select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.object_id = $1 order by name", [Id], Context),
        [ list_to_atom(binary_to_list(P)) || {P} <- Ps ]
    end,
    z_depcache:memo(F, {subject_preds, Id}, ?DAY, [Id], Context).

%% @doc Return the list of predicate ids in use by edges to objects from the id
%% @spec object_predicate_ids(Id, Context) -> List
object_predicate_ids(Id, Context) ->
    Ps = z_db:q("select distinct predicate_id from edge where subject_id = $1", [Id], Context),
    [ P || {P} <- Ps ].

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec subject_predicate_ids(Id, Context) -> List
subject_predicate_ids(Id, Context) ->
    Ps = z_db:q("select distinct predicate_id from edge where object_id = $1", [Id], Context),
    [ P || {P} <- Ps ].
