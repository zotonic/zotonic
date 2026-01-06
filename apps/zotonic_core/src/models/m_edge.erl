%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Model for accessing and manipulating edges between resources.
%% @end

%% Copyright 2009-2025 Marc Worrell
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
-moduledoc("
See also

[m\\_rsc](/id/doc_model_model_rsc), [m\\_media](/id/doc_model_model_media)

Access information about page connections.

Edges represent the connections between resources. They are implemented as tuples `{EdgeId, SubjectId, PredicateId, ObjectId,
OrderNr}`. The edge id is a unique id representing the edge, it can be used with edit actions. The OrderNr defines the
order of the edges with respect to the subject.

Most edge information is accessed using the [m\\_rsc](/id/doc_model_model_rsc) model, but some information can only
accessed with the m\\_edge model.

This model implements two template accessible options. They are mainly used to obtain the edge’s id for edit pages.

The following m\\_edge model properties are available in templates:

| Property  | Description                                                                      | Example value                                                                    |
| --------- | -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| o         | Returns a function that accepts a page  id and a predicate. The end result is a list of tuples \\\\{PageId, EdgeId\\\\} which are objects of the page. Example usage: `m.edge.o[id].author` | `[{204,13},{510,14}, {508,15}]`                                                  |
| s         | Identical to the “o” property, except that this function returns the subject edges. |                                                                                  |
| o\\\\_props | Similar to `m.edge.o[id].author` above, but returns a property list for the edges instead of the 2-tuple. | > \\\\[ > \\\\{id, 86062\\\\}, > \\\\{subject\\\\_id, 10635\\\\}, > \\\\{predicate\\\\_id, 304\\\\}, > \\\\{object\\\\_id, 57577\\\\}, > \\\\{seq, 1\\\\}, > \\\\{creator\\\\_id, 1\\\\}, > \\\\{created, \\\\{ > \\\\{2015,11,17\\\\}, > \\\\{11,23,32\\\\} > \\\\}\\\\} > \\\\] > \\\\] |
| s\\\\_props | Similar to `m.edge.s[id].author` above, but returns a property list for the edges instead of the 2-tuple. |                                                                                  |
| edges     | Returns a function that accepts a page id. The end result is a list of edges per predicate where the predicate is an atom and the edges are property lists. Example usage: `m.edge[10635]` | See example below.                                                               |
| id        | Look up an edge id by a subject/predicate/object triple. Example usage:   ```erlang m.edge.id[subject_id].relation[object_id] ```  or:   ```erlang m.edge.id[subject_id][predicate_name][object_id] ```  Returns `undefined` if the edge does not exist; otherwise returns an integer. | 213                                                                              |

Example return value for `{% print m.edge[10635] %}`:


```erlang
[{about,[[{id,86060},
          {subject_id,10635},
          {predicate_id,300},
          {name,<<\"about\">>},
          {object_id,17433},
          {seq,1},
          {created,{{2015,11,17},{11,22,11}}},
          {creator_id,1}]]},
 {author,[[{id,6},
           {subject_id,10635},
           {predicate_id,301},
           {name,<<\"author\">>},
           {object_id,10634},
           {seq,1000000},
           {created,{{2015,2,3},{16,23,20}}},
           {creator_id,1}]]}]
```



Other Topics
------------

`model/edge/post/o/+subject/+predicate/+object` or `model/edge/post/s/+object/+predicate/+object` inserts a new edge between resources.

The posted message can optionally include the name or id of the object, predicate and subject.


```javascript
cotonic.broker.publish(\"bridge/origin/edge/post/o/4312\",
                       {
                         predicate: \"author\",
                         subject: 7575
                       });
```

It is also possible to insert edges via cotonics onclick topics.


```django
<div data-onclick-topic=\"bridge/origin/model/edge/post/o/{{ id }}/?/{{ m.acl.user }}\">
   <button data-edge-predicate=\"is_going\">Is Going</button>
   <button data-edge-predicate=\"is_interested\">Might Go</button>
</div>
```

When a user clicks on a button, the model retrieves the predicate name (or id) from the `data-edge-predicate` attribute.
This is also possible by for the object and subject attributes of the edge. When there is a `?` in the topic path, the
value can be retrieved from a data attribute. The attribute value for object is: `data-edge-object`. For subject it is: `data-edge-subject`.

`model/edge/post/delete/o/+subject/+predicate/+object`, `model/edge/post/delete/s/+object/+predicate/+subject` or `model/edge/post/delete/edge/+edge_id` deletes the specified edge.


```javascript
cotonic.broker.publish(\"bridge/origin/edge/delete/edge/6776\");
```

Or via a onclick topic.


```django
<button data-onclick-topic=\"bridge/origin/edge/delete/{{ edge_id }}\">Delete</button
```
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    m_post/3,
    m_delete/3,

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
    has_objects/2,
    has_subjects/2,
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
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

-spec m_post( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:post_return().
m_post([<<"o">> | Path ], #{ payload := Payload }, Context) ->
    {Subject, Predicate, Object} = get_spo_o(Path, Payload, Context),
    do_post_insert(Subject, Predicate, Object, Context);
m_post([<<"s">> | Path ], #{ payload := Payload }, Context) ->
    {Subject, Predicate, Object} = get_spo_s(Path, Payload, Context),
    do_post_insert(Subject, Predicate, Object, Context).

-spec m_delete( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:delete_return().
m_delete([<<"o">> | Path], #{payload := Payload}, Context) ->
    {Subject, Predicate, Object} = get_spo_o(Path, Payload, Context),
    delete(Subject, Predicate, Object, Context);
m_delete([<<"s">> | Path], #{payload := Payload}, Context) ->
    {Subject, Predicate, Object} = get_spo_s(Path, Payload, Context),
    delete(Subject, Predicate, Object, Context);
m_delete([<<"edge">>, Edge], _Msg, Context) ->
    case z_convert:to_integer(Edge) of
        undefined ->
            {error, enoent};
        EdgeId ->
            delete(EdgeId, Context)
    end.

%% @doc Get the complete edge with the id
-spec get(EdgeId, Context) -> proplists:proplist() | undefined when
    EdgeId :: integer(),
    Context :: z:context().
get(Id, Context) ->
    z_db:assoc_row("select * from edge where id = $1", [Id], Context).

%% @doc Get the edge as a triple {subject_id, predicate, object_id}
-spec get_triple(EdgeId, Context) -> {SubjectId, Predicate, ObjectId} | undefined when
    EdgeId :: integer(),
    Context :: z:context(),
    SubjectId :: m_rsc:resource_id(),
    Predicate :: atom(),
    ObjectId :: m_rsc:resource_id().
get_triple(Id, Context) ->
    case z_db:q_row("
            select e.subject_id, r.name, e.object_id
            from edge e join rsc r on e.predicate_id = r.id
            where e.id = $1", [Id], Context)
    of
        {SubjectId, Predicate, ObjectId} ->
            {SubjectId, z_convert:to_atom(Predicate), ObjectId};
        undefined ->
            undefined
    end.

%% @doc Get the edge id of a subject/pred/object combination
-spec get_id(Subject, Predicate, Object, Context) -> EdgeId | undefined when
    Subject :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    Object :: m_rsc:resource(),
    Context :: z:context(),
    EdgeId :: pos_integer().
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
-spec get_edges(Subject, Context) -> PredicateEdges when
    Subject :: m_rsc:resource(),
    Context :: z:context(),
    PredicateEdges :: list( {Predicate, Edges}),
    Predicate :: atom(),
    Edges :: proplists:proplist().
get_edges(Subject, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            [];
        SubjectId ->
            case z_depcache:get({edges, SubjectId}, Context) of
                {ok, Edges} ->
                    Edges;
                undefined ->
                    Edges = z_db:assoc("
                        select e.id, e.subject_id, e.predicate_id, p.name, e.object_id,
                               e.seq, e.created, e.creator_id
                        from edge e join rsc p on p.id = e.predicate_id
                        where e.subject_id = $1
                        order by e.predicate_id, e.seq, e.id", [SubjectId], Context),
                    Edges1 = z_utils:group_proplists(name, Edges),
                    Edges2 = [ {z_convert:to_atom(Pred), Es} || {Pred, Es} <- Edges1 ],
                    z_depcache:set({edges, SubjectId}, Edges2, ?DAY, [SubjectId], Context),
                    Edges2
            end
    end.

%% @doc Insert a new edge. If the edge exists then the edge-id of te existing edge
%% is returned.
-spec insert(SubjectId, Predicate, ObjectId, Context) -> {ok, EdgeId} | {error, Reason} when
    SubjectId :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    ObjectId :: m_rsc:resource(),
    Context :: z:context(),
    EdgeId :: pos_integer(),
    Reason :: {unknown_predicate, m_rsc:resource()} | object | subject | eacces | unknown.
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
    case z_db:q1("
        select id
        from edge
        where subject_id = $1
          and object_id = $2
          and predicate_id = $3",
        [SubjectId, ObjectId, PredId],
        Context)
    of
        undefined ->
            {ok, PredName} = m_predicate:id_to_name(PredId, Context),
            case z_acl:is_allowed(insert,
                                  #acl_edge{subject_id=SubjectId, predicate=PredName, object_id=ObjectId},
                                  Context)
            of
                true ->
                    Created = case proplists:get_value(created, Opts) of
                        DT when is_tuple(DT) -> DT;
                        undefined -> undefined
                    end,
                    CreatorId = case proplists:get_value(creator_id, Opts) of
                        undefined -> z_acl:user(Context);
                        CId -> CId
                    end,
                    Transaction = fun(Ctx) ->
                        SeqOpt = maybe_seq_opt(Opts, SubjectId, PredId, Ctx),
                        insert_edge_1(SubjectId, ObjectId, PredId, SeqOpt, CreatorId, Created, Ctx)
                    end,
                    case z_db:transaction(Transaction, Context) of
                        {ok, 1, _, [{EdgeId}]} ->
                            z_edge_log_server:check(Context),
                            {ok, EdgeId};
                        {ok, 0, _, []} ->
                            % Race condition, edge might have been inserted by another transaction
                            case z_db:q1("
                                select id
                                from edge
                                where subject_id = $1
                                  and object_id = $2
                                  and predicate_id = $3",
                                [SubjectId, ObjectId, PredId],
                                Context)
                            of
                                undefined ->
                                    % Some other error during insert -- should not happen
                                    ?LOG_ERROR(#{
                                        in => zotonic_core,
                                        text => <<"Error inserting edge">>,
                                        result => error,
                                        reason => unknown,
                                        subject_id => SubjectId,
                                        predicate_id => PredId,
                                        object_id => ObjectId,
                                        creator_id => CreatorId,
                                        created => Created
                                    }),
                                    {error, unknown};
                                EdgeId ->
                                    {ok, EdgeId}
                            end;
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => zotonic_core,
                                text => <<"Error inserting edge">>,
                                result => error,
                                reason => Reason,
                                subject_id => SubjectId,
                                predicate_id => PredId,
                                object_id => ObjectId,
                                creator_id => CreatorId,
                                created => Created
                            }),
                            {error, enoent}
                    end;
                false ->
                    {error, eacces}
            end;
        EdgeId ->
            % Edge exists - skip
            {ok, EdgeId}
end.

insert_edge_1(SubjectId, ObjectId, PredId, undefined, CreatorId, undefined, Context) ->
    z_db:equery("
        insert into edge
            (subject_id, object_id, predicate_id, creator_id)
        values
            ($1, $2, $3, $4)
        on conflict do nothing
        returning id
        ",
        [
            SubjectId,
            ObjectId,
            PredId,
            CreatorId
        ],
        Context);
insert_edge_1(SubjectId, ObjectId, PredId, Seq, CreatorId, undefined, Context) ->
    z_db:equery("
        insert into edge
            (subject_id, object_id, predicate_id, seq, creator_id)
        values
            ($1, $2, $3, $4, $5)
        on conflict do nothing
        returning id
        ",
        [
            SubjectId,
            ObjectId,
            PredId,
            Seq,
            CreatorId
        ],
        Context);
insert_edge_1(SubjectId, ObjectId, PredId, undefined, CreatorId, Created, Context) ->
    z_db:equery("
        insert into edge
            (subject_id, object_id, predicate_id, creator_id, created)
        values
            ($1, $2, $3, $4, $5)
        on conflict do nothing
        returning id
        ",
        [
            SubjectId,
            ObjectId,
            PredId,
            CreatorId,
            Created
        ],
        Context);
insert_edge_1(SubjectId, ObjectId, PredId, SeqOpt, CreatorId, Created, Context) ->
    z_db:equery("
        insert into edge
            (subject_id, object_id, predicate_id, seq, creator_id, created)
        values
            ($1, $2, $3, $4, $5, $6)
        on conflict do nothing
        returning id
        ",
        [
            SubjectId,
            ObjectId,
            PredId,
            SeqOpt,
            CreatorId,
            Created
        ],
        Context).

%% @doc Determine the sequence number for the new edge. Needed for imports with
%% specific sequence order, or if the "is_insert_before" option is used or set for
%% the predicate.
maybe_seq_opt(Opts, SubjectId, PredId, Context) ->
    case proplists:get_value(seq, Opts) of
        S when is_integer(S) ->
            S;
        _ ->
            case z_convert:to_bool( proplists:get_value(is_insert_before, Opts) )
                orelse z_convert:to_bool( m_rsc:p_no_acl(PredId, <<"is_insert_before">>, Context) )
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
                        undefined -> undefined;
                        N -> N-1
                    end;
                false ->
                    undefined
            end
    end.

%% @doc Delete an edge by Id. If the edge doesn't exist then 'ok' is returned.
-spec delete(EdgeId, Context) -> ok | {error, Reason} when
    EdgeId :: integer(),
    Context :: z:context(),
    Reason :: eacces.
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

%% @doc Delete an edge by subject, object and predicate id. Returns ok
%% if the edge is deleted or didn't exist.
-spec delete(Subject, Predicate, Object, Context) -> ok | {error, Reason} when
    Subject :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    Object :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: eacces | object | subject | predicate.
delete(Subject, Predicate, Object, Context) ->
    delete(Subject, Predicate, Object, [], Context).

%% @doc Delete an edge by subject, object and predicate id. Options are ignored. Returns ok
%% if the edge is deleted or didn't exist.
-spec delete(Subject, Predicate, Object, Options, Context) -> ok | {error, Reason} when
    Subject :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    Object :: m_rsc:resource(),
    Options :: list(),
    Context :: z:context(),
    Reason :: eacces | object | subject | predicate.
delete(SubjectId, Pred, ObjectId, _Options, Context) when is_integer(SubjectId), is_integer(ObjectId) ->
    case to_predicate(Pred, Context) of
        {ok, PredId} ->
            {ok, PredName} = m_predicate:id_to_name(PredId, Context),
            case z_acl:is_allowed(
                delete,
                #acl_edge{ subject_id = SubjectId, predicate = PredName, object_id = ObjectId },
                Context
            ) of
                true ->
                    _ = z_db:q("
                        delete from edge
                        where subject_id = $1
                          and object_id = $2
                          and predicate_id = $3
                        ",
                        [SubjectId, ObjectId, PredId],
                        Context),
                    z_edge_log_server:check(Context),
                    ok;
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end;
delete(SubjectId, Pred, Object, Options, Context) when not is_integer(Object) ->
    case m_rsc:rid(Object, Context) of
        undefined -> {error, object};
        Id -> delete(SubjectId, Pred, Id, Options, Context)
    end;
delete(Subject, Pred, Object, Options, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined -> {error, subject};
        Id -> delete(Id, Pred, Object, Options, Context)
    end.

to_predicate(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, predicate};
        RId ->
            case m_rsc:is_a(RId, predicate, Context) of
                true ->
                    {ok, RId};
                false ->
                    {error, predicate}
            end
    end.



%% @doc Delete multiple edges between the subject and the object. Invalid predicates are
%% ignored.
-spec delete_multiple(Subject, Predicates, Object, Context) -> ok | {error, Reason} when
    Subject :: m_rsc:resource(),
    Predicates :: [ m_rsc:resource() ],
    Object :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: eacces | subject | object.
delete_multiple(SubjectId, Predicates, ObjectId, Context) when is_integer(SubjectId), is_integer(ObjectId) ->
    PredIds = lists:map(
        fun(Predicate) ->
            {ok, Id} = m_predicate:name_to_id(Predicate, Context),
            Id
        end,
        Predicates),
    PredNames = [ m_predicate:id_to_name(PredId, Context) || PredId <- PredIds ],
    IsAllowed = lists:all(
        fun
            ({ok, PredName}) ->
                z_acl:is_allowed(
                    delete,
                    #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
                    Context);
            ({error, _}) ->
                true
        end,
        PredNames),
    case IsAllowed of
        true ->
            Count = z_db:q("delete
                    from edge
                    where subject_id = $1
                      and object_id = $2
                      and predicate_id = any($3::int[])",
                [SubjectId, ObjectId, PredIds], Context),

            case Count of
                0 ->
                    ok;
                N when is_integer(N) ->
                    z_edge_log_server:check(Context),
                    ok
            end;
        false ->
            {error, eacces}
    end;
delete_multiple(SubjectId, Pred, Object, Context) when is_integer(SubjectId) ->
    case m_rsc:rid(Object, Context) of
        undefined -> {error, object};
        Id -> delete_multiple(SubjectId, Pred, Id, Context)
    end;
delete_multiple(Subject, Pred, Object, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined -> {error, subject};
        Id -> delete_multiple(Id, Pred, Object, Context)
    end.


%% @doc Replace the objects with the new list
-spec replace(m_rsc:resource(), m_rsc:resource(), [ m_rsc:resource() ], z:context()) -> ok | {error, atom()}.
replace(SubjectId, PredId, NewObjects, Context) when is_integer(PredId), is_integer(SubjectId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> replace1(SubjectId, PredId, NewObjects, Context);
        false -> {error, {unknown_predicate, PredId}}
    end;
replace(SubjectId, Predicate, NewObjects, Context) when not is_integer(Predicate) ->
    {ok, PredId} = m_predicate:name_to_id(Predicate, Context),
    replace1(SubjectId, PredId, NewObjects, Context);
replace(Subject, Predicate, NewObjects, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined -> {error, subject};
        Id -> replace(Id, Predicate, NewObjects, Context)
    end.

replace1(SubjectId, PredId, NewObjects0, Context) ->
    NewObjects = lists:sort(lists:filtermap(
        fun(Obj) ->
            case m_rsc:rid(Obj, Context) of
                undefined -> false;
                OId -> {true, OId}
            end
        end,
        NewObjects0)),
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    case lists:sort(objects(SubjectId, PredId, Context)) of
        NewObjects ->
            ok;

        CurrObjects ->
            % Check the ACL for insertion and deletion
            IsAllowed1 = lists:all(
                fun(ObjectId) ->
                    z_acl:is_allowed(
                        delete,
                        #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
                        Context)
                end,
                CurrObjects -- NewObjects),
            IsAllowed2 = lists:all(
                fun(ObjectId) ->
                    z_acl:is_allowed(insert,
                        #acl_edge{subject_id = SubjectId, predicate = PredName, object_id = ObjectId},
                        Context)
                end,
                NewObjects -- CurrObjects),

            case IsAllowed1 andalso IsAllowed2 of
                true ->
                    Result = set_sequence(SubjectId, PredId, NewObjects, Context),
                    z_edge_log_server:check(Context),
                    Result;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Duplicate all edges from one subject to another subject. Skip all edges that give
%% ACL errors. Return eacces if the source or target subject is not editable.
-spec duplicate(FromId, ToId, Context) -> ok | {error, Reason} when
    FromId :: m_rsc:resource(),
    ToId :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: eacces.
duplicate(FromId, ToId, Context) ->
    case z_acl:rsc_editable(FromId, Context) andalso z_acl:rsc_editable(ToId, Context) of
        true ->
            F = fun(Ctx) ->
                FromEdges = z_db:q("
                                select predicate_id, object_id, seq
                                from edge
                                where subject_id = $1
                                order by seq, id",
                    [m_rsc:rid(FromId, Context)],
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
            {error, eacces}
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
-spec update_nth(SubjectId, Predicate, Nth, ObjectId, Context) -> {ok, EdgeId} | {error, Reason} when
    SubjectId :: m_rsc:resource_id(),
    Predicate :: m_rsc:resource(),
    Nth :: integer(),
    ObjectId :: m_rsc:resource_id(),
    EdgeId :: pos_integer(),
    Context :: z:context(),
    Reason :: eacces | enoent | term().
update_nth(SubjectId, Predicate, Nth, ObjectId, Context) when is_integer(SubjectId), is_integer(ObjectId) ->
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
    end;
update_nth(Subject, Predicate, Nth, ObjectId, Context) when not is_integer(Subject) ->
    case m_rsc:rid(Subject, Context) of
        undefined -> {error, subject};
        Id -> update_nth(Id, Predicate, Nth, ObjectId, Context)
    end;
update_nth(SubjectId, Predicate, Nth, Object, Context) ->
    case m_rsc:rid(Object, Context) of
        undefined -> {error, object};
        Id -> update_nth(SubjectId, Predicate, Nth, Id, Context)
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
-spec objects( m_rsc:resource(), z:context() ) -> [ m_rsc:resource_id() ].
objects(Subject, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            [];
        SubjectId ->
            F = fun() ->
                Ids = z_db:q(
                    "select object_id from edge "
                    "where subject_id = $1 "
                    "order by predicate_id, seq, id",
                    [SubjectId],
                    Context
                ),
                [ObjId || {ObjId} <- Ids]
            end,
            z_depcache:memo(F, {objects, SubjectId}, ?DAY, [SubjectId], Context)
    end.

%% @doc Return all subject ids of the resource
-spec subjects( m_rsc:resource(), z:context() ) -> [ m_rsc:resource_id() ].
subjects(Object, Context) ->
    case m_rsc:rid(Object, Context) of
        undefined ->
            [];
        ObjectId ->
            F = fun() ->
                Ids = z_db:q(
                    "select subject_id from edge "
                    "where object_id = $1 order by predicate_id, id",
                    [ObjectId],
                    Context
                ),
                [SubjId || {SubjId} <- Ids]
            end,
            z_depcache:memo(F, {subjects, ObjectId}, ?HOUR, [ObjectId], Context)
    end.

%% @doc Check if a resource has object edges, no caching is done.
-spec has_objects(m_rsc:resource(), z:context()) -> boolean().
has_objects(Subject, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            false;
        SubjectId ->
            is_integer(
                z_db:q1("
                    select id from edge
                    where subject_id = $1
                    limit 1",
                    [SubjectId],
                    Context))
    end.

%% @doc Check if a resource has subject edges, no caching is done.
-spec has_subjects(m_rsc:resource(), z:context()) -> boolean().
has_subjects(Object, Context) ->
    case m_rsc:rid(Object, Context) of
        undefined ->
            false;
        ObjectId ->
            is_integer(
                z_db:q1("
                    select id from edge
                    where object_id = $1
                    limit 1",
                    [ObjectId],
                    Context))
    end.

%% @doc Return all object ids with the edge id for a predicate/subject_id
-spec object_edge_ids( m_rsc:resource(), m_rsc:resource(), z:context() ) -> [ {m_rsc:resource_id(), integer()} ].
object_edge_ids(Subject, Predicate, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            [];
        SubjectId ->
            case m_predicate:name_to_id(Predicate, Context) of
                {ok, PredId} ->
                    F = fun() ->
                        z_db:q(
                            "select object_id, id from edge "
                            "where subject_id = $1 and predicate_id = $2 "
                            "order by seq, id",
                            [SubjectId, PredId],
                            Context
                        )
                    end,
                    z_depcache:memo(F, {object_edge_ids, SubjectId, PredId}, ?DAY, [SubjectId], Context);
                {error, _} ->
                    []
            end
    end.


%% @doc Return all subject ids with the edge id for a predicate/object_id
-spec subject_edge_ids( m_rsc:resource(), m_rsc:resource(), z:context() ) -> [ {m_rsc:resource_id(), integer()} ].
subject_edge_ids(Object, Predicate, Context) ->
    case m_rsc:rid(Object, Context) of
        undefined ->
            [];
        ObjectId ->
            case m_predicate:name_to_id(Predicate, Context) of
                {ok, PredId} ->
                    F = fun() ->
                        z_db:q(
                            "select subject_id, id from edge "
                            "where object_id = $1 and predicate_id = $2 "
                            "order by seq, id",
                            [ObjectId, PredId],
                            Context
                        )
                    end,
                    z_depcache:memo(F, {subject_edge_ids, ObjectId, PredId}, ?DAY, [ObjectId], Context);
                {error, _} ->
                    []
            end
    end.


%% @doc Return all object ids with edge properties
-spec object_edge_props(m_rsc:resource(), m_rsc:resource(), z:context()) -> list( m_rsc:resource_id() ).
object_edge_props(Subject, Predicate, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            [];
        SubjectId ->
            case m_predicate:name_to_id(Predicate, Context) of
                {ok, PredId} ->
                    F = fun() ->
                        z_db:assoc("
                                select *
                                from edge
                                where subject_id = $1
                                  and predicate_id = $2
                                order by seq, id",
                            [SubjectId, PredId],
                            Context)
                    end,
                    z_depcache:memo(F, {object_edge_props, SubjectId, PredId}, ?DAY, [SubjectId], Context);
                {error, _} ->
                    []
            end
    end.

%% @doc Return all subject ids with the edge properties
-spec subject_edge_props(m_rsc:resource(), m_rsc:resource(), z:context()) -> list( m_rsc:resource_id() ).
subject_edge_props(Object, Predicate, Context) ->
    case m_rsc:rid(Object, Context) of
        undefined ->
            [];
        ObjectId ->
            case m_predicate:name_to_id(Predicate, Context) of
                {ok, PredId} ->
                    F = fun() ->
                        z_db:assoc("
                                select *
                                from edge
                                where object_id = $1
                                  and predicate_id = $2
                                order by seq, id",
                            [ObjectId, PredId],
                            Context)
                    end,
                    z_depcache:memo(F, {subject_edge_props, ObjectId, PredId}, ?DAY, [ObjectId], Context);
                {error, _} ->
                    []
            end
    end.


%% @doc Reorder the edges so that the mentioned ids are in front, in the listed order.
-spec update_sequence( m_rsc:resource(), m_rsc:resource(), [ m_rsc:resource_id() ], z:context() ) ->
    ok | {error, term()}.
update_sequence(Subject, Pred, ObjectIds, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            {error, enoent};
        SubjectId ->
            {ok, PredId} = m_predicate:name_to_id(Pred, Context),
            case z_acl:rsc_editable(SubjectId, Context) of
                true ->
                    F = fun(Ctx) ->
                        All = z_db:q("
                                    select object_id, id
                                    from edge
                                    where predicate_id = $1
                                      and subject_id = $2", [PredId, SubjectId], Ctx),

                        MissingIds = lists:foldl(
                            fun({OId, _}, Acc) ->
                                case lists:member(OId, ObjectIds) of
                                    true -> Acc;
                                    false -> [OId | Acc]
                                end
                            end,
                            [],
                            All),

                        lists:map(
                            fun(OId) ->
                                case lists:keymember(OId, 1, All) of
                                    true ->
                                        ok;
                                    false ->
                                        z_db:q("
                                            insert into edge (subject_id, predicate_id, object_id)
                                            values ($1, $2, $3)",
                                            [ SubjectId, PredId, OId ],
                                            Context)
                                end
                            end,
                            ObjectIds),

                        SortedIds = ObjectIds ++ lists:reverse(MissingIds),
                        SortedEdgeIds = [proplists:get_value(OId, All, -1) || OId <- SortedIds],
                        z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                        ok
                    end,

                    Result = z_db:transaction(F, Context),
                    z_depcache:flush(SubjectId, Context),
                    z_depcache:flush({predicate, PredId}, Context),
                    z_edge_log_server:check(Context),
                    Result;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Set edges order so that the specified object ids are in given order.
%% Any extra edges not specified will be deleted, and any missing edges will be inserted.
-spec set_sequence( m_rsc:resource(), m_rsc:resource(), [ m_rsc:resource_id() ], z:context()) -> ok | {error, term()}.
set_sequence(Subject, Pred, ObjectIds, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            {error, enoent};
        SubjectId ->
            case z_acl:rsc_editable(SubjectId, Context) of
                true ->
                    case m_predicate:name_to_id(Pred, Context) of
                        {ok, PredId} ->
                            F = fun(Ctx) ->
                                All = z_db:q("
                                            select object_id, id
                                            from edge
                                            where predicate_id = $1
                                              and subject_id = $2", [PredId, SubjectId], Ctx),

                                % Delete edges not in ObjectIds
                                lists:foreach(
                                    fun({ObjectId, EdgeId}) ->
                                        case lists:member(ObjectId, ObjectIds) of
                                            true ->
                                                ok;
                                            false ->
                                                delete(EdgeId, Context)
                                        end
                                    end,
                                    All),

                                % Add new edges not yet in the db
                                NewEdges = lists:filtermap(
                                    fun(ObjectId) ->
                                        case lists:member(ObjectId, All) of
                                            true -> false;
                                            false ->
                                                case insert(SubjectId, Pred, ObjectId, Context) of
                                                    {ok, EdgeId} ->
                                                        {true, {ObjectId, EdgeId}};
                                                    {error, _} ->
                                                        false
                                                end
                                        end
                                    end,
                                    ObjectIds),

                                % Force order of all edges
                                AllEdges = All ++ NewEdges,
                                SortedEdgeIds = [
                                    proplists:get_value(OId, AllEdges, -1) || OId <- ObjectIds
                                ],
                                z_db:update_sequence(edge, SortedEdgeIds, Ctx),
                                ok
                            end,

                            Result = z_db:transaction(F, Context),
                            z_depcache:flush(SubjectId, Context),
                            z_depcache:flush({predicate, PredId}, Context),
                            z_edge_log_server:check(Context),
                            Result;
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end.


%% @doc Update the sequence for the given edge ids.  Optionally rename the predicate on the edge.
-spec update_sequence_edge_ids( m_rsc:resource_id(), m_rsc:resource(), [ integer() ], z:context() ) -> ok | {error, term()}.
update_sequence_edge_ids(Subject, Pred, EdgeIds, Context) ->
    case m_rsc:rid(Subject, Context) of
        undefined ->
            {error, enoent};
        Id ->
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
                    z_depcache:flush({predicate, PredId}, Context),
                    z_edge_log_server:check(Context),
                    Result;
                false ->
                    {error, eacces}
            end
    end.


%% @doc Return the list of predicates in use by edges to objects from the id
-spec object_predicates( m_rsc:resource_id(), z:context() ) -> list( atom() ).
object_predicates(Id, Context) when is_integer(Id) ->
    F = fun() ->
        Ps = z_db:q(
            "select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.subject_id = $1 "
            "order by name", [Id], Context),
        [list_to_atom(binary_to_list(P)) || {P} <- Ps]
    end,
    z_depcache:memo(F, {object_preds, Id}, ?DAY, [Id], Context).

%% @doc Return the list of predicates is use by edges from subjects to the id
-spec subject_predicates( m_rsc:resource_id(), z:context() ) -> list( atom() ).
subject_predicates(Id, Context) when is_integer(Id) ->
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
-spec object_predicate_ids( m_rsc:resource_id(), z:context() ) -> list( m_rsc:resource_id() ).
object_predicate_ids(Id, Context) when is_integer(Id) ->
    Ps = z_db:q("select distinct predicate_id from edge where subject_id = $1", [Id], Context),
    [P || {P} <- Ps].

%% @doc Return the list of predicates is use by edges from subjects to the id
-spec subject_predicate_ids( m_rsc:resource_id(), z:context() ) -> list( m_rsc:resource_id() ).
subject_predicate_ids(Id, Context) when is_integer(Id) ->
    Ps = z_db:q("select distinct predicate_id from edge where object_id = $1", [Id], Context),
    [P || {P} <- Ps].

%%
%% Helpers
%%

do_post_insert(Subject, Predicate, Object, Context) ->
    case insert(Subject, Predicate, Object, Context) of
        {ok, Id} ->
            {ok, #{ id => Id }};
        {error, _}=Error ->
            Error
    end.

get_spo_o(Path, Payload, Context) ->
    Subject = get_from_path_or_payload(1, <<"subject">>, Path, Payload, Context),
    Predicate = get_from_path_or_payload(2, <<"predicate">>, Path, Payload, Context),
    Object = get_from_path_or_payload(3, <<"object">>, Path, Payload, Context),
    {Subject, Predicate, Object}.

get_spo_s(Path, Payload, Context) ->
    Object = get_from_path_or_payload(1, <<"object">>, Path, Payload, Context),
    Predicate = get_from_path_or_payload(2, <<"predicate">>, Path, Payload, Context),
    Subject = get_from_path_or_payload(3, <<"subject">>, Path, Payload, Context),
    {Subject, Predicate, Object}.

get_from_path_or_payload(1, _What, [Thing | _Rest ], _Payload, _Context) when Thing =/= <<"?">> -> Thing;
get_from_path_or_payload(2, _What, [_, Thing | _Rest ], _Payload, _Context) when Thing =/= <<"?">> -> Thing;
get_from_path_or_payload(3, _What, [_, _, Thing | _Rest ], _Payload, _Context) when Thing =/= <<"?">> -> Thing;
get_from_path_or_payload(_, What, _Path, Payload, Context) ->
    get_q(What, Payload, Context).

get_q(Name, Payload, Context) ->
    case maps:get(Name, Payload, undefined) of
        undefined ->
            case maps:get(<<"data-edge-", Name/binary>>, maps:get(<<"message">>, Payload, #{}), undefined) of
                undefined ->
                    z_context:get_q(Name, Context);
                Value ->
                    Value
            end;
        Value ->
            Value
    end.

