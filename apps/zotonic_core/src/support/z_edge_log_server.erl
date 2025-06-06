%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2025 Marc Worrell
%% @doc Check for changed edges, trigger notifications.
%% @end

%% Copyright 2014-2025 Marc Worrell
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

-module(z_edge_log_server).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    check/1,
    maybe_schedule_dependent_check/2,
    delete_if_unconnected/2
]).

-export([
    observe_check_edge_log/2
]).

-include_lib("zotonic.hrl").

% Check every 10 minutes if we have anything to handle.
% Check every 100msec when working through a backlog.
-define(CLEANUP_TIMEOUT_LONG, 600000).
-define(CLEANUP_TIMEOUT_SHORT, 100).
-define(CLEANUP_BATCH_SIZE, 100).

-record(state, {site :: atom()}).


%% @doc Schedule a check if the resource is connected, if not then then the resource will be deleted.
%% This is called after a resource is inserted. The check is done 12 hours after the resource has
%% been inserted.
-spec maybe_schedule_dependent_check(Id, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    Reason :: term().
maybe_schedule_dependent_check(Id, Context) ->
    maybe_schedule_dependent_check(Id, 12*3600, Context).


%% @doc Force a check, useful after known edge operations.
check(Context) ->
    z_notifier:notify(check_edge_log, Context).

%% @doc Do an edge log check.
observe_check_edge_log(check_edge_log, Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:cast(Name, check).

%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
-spec start_link(atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(Site) ->
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    z_notifier:observe(check_edge_log, {?MODULE, observe_check_edge_log}, Site),
    {ok, #state{site=Site}, ?CLEANUP_TIMEOUT_LONG}.

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(check, State) ->
    case do_check(State#state.site) of
        {ok, 0} ->
            {noreply, State, ?CLEANUP_TIMEOUT_LONG};
        {ok, _} ->
            {noreply, State, ?CLEANUP_TIMEOUT_SHORT};
        {error, _} ->
            {noreply, State, ?CLEANUP_TIMEOUT_LONG}
    end;

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @doc Handling all non call/cast messages
handle_info(timeout, State) ->
    handle_cast(check, State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

-spec do_check( atom() ) -> {ok, non_neg_integer()} | {error, econnrefused}.
do_check(Site) ->
    try
        Context = z_acl:sudo(z_context:new(Site)),
        do_check_1(z_db:q("
                        select id,op,subject_id,predicate,object_id,edge_id
                        from edge_log
                        order by id
                        limit $1",
                        [?CLEANUP_BATCH_SIZE],
                        Context),
                   Context)
    catch
        exit:{timeout, _} -> {ok, 0};
        throw:{error, econnrefused} = Error -> Error
    end.


do_check_1([], _Context) ->
    {ok, 0};
do_check_1(Rs, Context) ->
    RscIds = lists:usort(fetch_ids(Rs, [])),
    lists:foreach(fun(RscId) ->
                    z_depcache:flush(RscId, Context)
                  end,
                  RscIds),
    Ns = lists:foldl(
        fun({_Id,Op,SubjectId,Predicate,ObjectId,EdgeId}, Acc) ->
            PredName = z_convert:to_atom(Predicate),
            z_depcache:flush({predicate, m_rsc:rid(PredName, Context)}, Context),
            do_edge_notify(Op, SubjectId, PredName, ObjectId, EdgeId, Acc, Context)
        end,
        #{},
        Rs),
    maps:fold(
        fun(Topic, Events, ok) ->
            z_mqtt:publish(Topic, lists:reverse(Events), Context)
        end,
        ok,
        Ns),
    Ranges = z_utils:ranges([ element(1,R) || R <- Rs ]),
    z_db:transaction(
            fun(Ctx) ->
                lists:foreach(fun
                                ({A,A}) ->
                                    z_db:q("delete from edge_log where id = $1", [A], Ctx);
                                ({A,B}) ->
                                    z_db:q("delete from edge_log where id >= $1 and id <= $2", [A,B], Ctx)
                              end,
                              Ranges)
             end,
             Context),
    {ok, length(Rs)}.

fetch_ids([], Acc) ->
    Acc;
fetch_ids([{_Id,_Op,SubjectId,_Pred,ObjectId,_EdgeId}|Rs], Acc) ->
    fetch_ids(Rs, [SubjectId,ObjectId|Acc]).

do_edge_notify(<<"DELETE">>, SubjectId, PredName, ObjectId, EdgeId, Acc, Context) ->
    Edge = #edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId, edge_id=EdgeId},
    z_notifier:notify_sync(Edge, Context),
    maybe_delete_dependent(ObjectId, Context),
    T1 = [ <<"model">>, <<"edge">>, <<"event">>, SubjectId, <<"o">>, z_convert:to_binary(PredName) ],
    T2 = [ <<"model">>, <<"edge">>, <<"event">>, ObjectId, <<"s">>, z_convert:to_binary(PredName) ],
    Acc#{
        T1 => [ Edge | maps:get(T1, Acc, []) ],
        T2 => [ Edge | maps:get(T2, Acc, []) ]
    };
do_edge_notify(<<"UPDATE">>, SubjectId, PredName, ObjectId, EdgeId, Acc, Context) ->
    Edge = #edge_update{subject_id=SubjectId, predicate=PredName, object_id=ObjectId, edge_id=EdgeId},
    z_notifier:notify_sync(Edge, Context),
    T1 = [ <<"model">>, <<"edge">>, <<"event">>, SubjectId, <<"o">>, z_convert:to_binary(PredName) ],
    T2 = [ <<"model">>, <<"edge">>, <<"event">>, ObjectId, <<"s">>, z_convert:to_binary(PredName) ],
    Acc#{
        T1 => [ Edge | maps:get(T1, Acc, []) ],
        T2 => [ Edge | maps:get(T2, Acc, []) ]
    };
do_edge_notify(<<"INSERT">>, SubjectId, PredName, ObjectId, EdgeId, Acc, Context) ->
    Edge = #edge_insert{subject_id=SubjectId, predicate=PredName, object_id=ObjectId, edge_id=EdgeId},
    z_notifier:notify_sync(Edge, Context),
    T1 = [ <<"model">>, <<"edge">>, <<"event">>, SubjectId, <<"o">>, z_convert:to_binary(PredName) ],
    T2 = [ <<"model">>, <<"edge">>, <<"event">>, ObjectId, <<"s">>, z_convert:to_binary(PredName) ],
    Acc#{
        T1 => [ Edge | maps:get(T1, Acc, []) ],
        T2 => [ Edge | maps:get(T2, Acc, []) ]
    }.

%% @doc After edge deletion, check if a dependent resource is connected. If not then the resource
%% can be deleted. There is a waiting period of an hour so that a resource can be disconnected first
%% before being connected to another resource.
maybe_delete_dependent(Id, Context) ->
    maybe_schedule_dependent_check(Id, 3600, Context).

maybe_schedule_dependent_check(Id, Delay, Context) when is_integer(Id) ->
    case m_rsc:p_no_acl(Id, <<"is_dependent">>, Context) of
        true ->
            Key = z_convert:to_binary(Id),
            case m_edge:has_subjects(Id, Context) of
                true -> ok;
                false ->
                    case z_pivot_rsc:insert_task_after(Delay, ?MODULE, delete_if_unconnected, Key, [Id], Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end
            end;
        _False ->
            ok
    end.

delete_if_unconnected(Id, Context) ->
    case z_db:q_row("select r.is_dependent, r.is_protected, e.object_id
                     from rsc r
                          left join edge e on e.object_id = r.id
                     where r.is_dependent
                       and r.id = $1
                     limit 1",
                    [Id], Context)
    of
        {true, false, undefined} -> m_rsc:delete(Id, z_acl:sudo(Context));
        _ -> ok
    end.
