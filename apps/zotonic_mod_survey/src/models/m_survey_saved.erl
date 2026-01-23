%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Model for accessing intermediate survey answers for users.
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

-module(m_survey_saved).
-moduledoc("
Model for saving and retrieving intermediate survey answers for users. There can
only be a single intermediate result per user/survey or, for anonymous users per
persistent-id/survey. The intermediate results are deleted when the survey is
finished.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).


%% interface functions
-export([
    m_get/3,

    delete_saved/2,
    delete_saved_user/3,
    delete_saved_persistent/3,

    has_saved/2,
    has_saved_user/3,
    has_saved_persistent/3,

    get_saved/2,
    get_saved_user/3,
    get_saved_persistent/3,

    put_saved/4,
    put_saved_user/5,
    put_saved_persistent/5,

    prune_saved/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([ <<"get_saved">>, SurveyId | Rest ], _Msg, Context) ->
    case get_saved(m_rsc:rid(SurveyId, Context), Context) of
        {ok, Ans} ->
            {ok, {Ans, Rest}};
        {error, _} ->
            {error, enoent}
    end;
m_get([ <<"has_saved">>, SurveyId | Rest ], _Msg, Context) ->
    {ok, {has_saved(m_rsc:rid(SurveyId, Context), Context), Rest}}.

%% @doc Store the intermediate answers for the current user or the persistent
%% id from the user agent.
-spec put_saved(SurveyId, PageNr, Answers, Context) -> ok | {error, Reason} when
    SurveyId :: m_rsc:resource() | undefined,
    PageNr :: pos_integer(),
    Answers :: list(),
    Context :: z:context(),
    Reason :: enoent | term().
put_saved(undefined, _PageNr, _Answers, _Context) ->
    {error, enoent};
put_saved(SurveyId, PageNr, Answers, Context) when is_integer(SurveyId) ->
    case z_acl:user(Context) of
        undefined ->
            case m_client_local_storage:fetch_device_id(Context) of
                {ok, PersistentNr} ->
                    put_saved_persistent(SurveyId, PersistentNr, PageNr, Answers, Context);
                {error, _} ->
                    {error, enoent}
            end;
        UserId ->
            put_saved_user(SurveyId, UserId, PageNr, Answers, Context)
    end;
put_saved(SurveyId, PageNr, Answers, Context) ->
    put_saved(m_rsc:rid(SurveyId, Context), PageNr, Answers, Context).


-spec put_saved_user(SurveyId, UserId, PageNr, Answers, Context) -> ok | {error, Reason} when
    SurveyId :: m_rsc:resource() | undefined,
    UserId :: m_rsc:resource_id(),
    PageNr :: pos_integer(),
    Answers :: list(),
    Context :: z:context(),
    Reason :: enoent | term().
put_saved_user(SurveyId, UserId, PageNr, Answers, Context) ->
    _ = z_db:q("
        insert into survey_answers_saved
            (survey_id, user_id, page_nr, saved_args, modified)
        values
            ($1, $2, $3, $4, now())
        on conflict (survey_id, user_id)
        do update set
            page_nr = $3,
            saved_args = $4,
            modified = now()
        ",
        [SurveyId, UserId, PageNr, ?DB_PROPS(Answers)],
        Context),
    ok.


-spec put_saved_persistent(SurveyId, PersistentNr, PageNr, Answers, Context) -> ok | {error, Reason} when
    SurveyId :: m_rsc:resource() | undefined,
    PersistentNr :: binary(),
    PageNr :: pos_integer(),
    Answers :: list(),
    Context :: z:context(),
    Reason :: enoent | term().
put_saved_persistent(SurveyId, PersistentNr, PageNr, Answers, Context) ->
    _ = z_db:q("
        insert into survey_answers_saved
            (survey_id, persistent, page_nr, saved_args, modified)
        values
            ($1, $2, $3, $4, now())
        on conflict (survey_id, persistent)
        do update set
            page_nr = $3,
            saved_args = $4,
            modified = now()
        ",
        [SurveyId, PersistentNr, PageNr, ?DB_PROPS(Answers)],
        Context),
    ok.


%% @doc Fetch the intermediate answers for the current user or the persistent
%% id from the user agent.
-spec get_saved(SurveyId, Context) -> {ok, Data} | {error, Reason} when
    SurveyId :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Data :: map(),
    Reason :: enoent | term().
get_saved(undefined, _Context) ->
    {error, enoent};
get_saved(SurveyId, Context) when is_integer(SurveyId) ->
    case z_acl:user(Context) of
        undefined ->
            case m_client_local_storage:fetch_device_id(Context) of
                {ok, PersistentNr} ->
                    get_saved_persistent(SurveyId, PersistentNr, Context);
                {error, _} ->
                    {error, enoent}
            end;
        UserId ->
            get_saved_user(SurveyId, UserId, Context)
    end;
get_saved(SurveyId, Context) ->
    get_saved(m_rsc:rid(SurveyId, Context), Context).


get_saved_user(SurveyId, UserId, Context) ->
    z_db:qmap_row("
        select *
        from survey_answers_saved
        where survey_id = $1
          and user_id = $2",
        [SurveyId, UserId],
        Context).

get_saved_persistent(SurveyId, PersistentNr, Context) ->
    z_db:qmap_row("
        select *
        from survey_answers_saved
        where survey_id = $1
          and persistent = $2",
        [SurveyId, PersistentNr],
        Context).


%% @doc Check if there are intermediate answers for the current user or the persistent
%% id from the user agent.
-spec has_saved(SurveyId, Context) -> boolean() when
    SurveyId :: m_rsc:resource() | undefined,
    Context :: z:context().
has_saved(undefined, _Context) ->
    false;
has_saved(SurveyId, Context) when is_integer(SurveyId) ->
    case z_acl:user(Context) of
        undefined ->
            case m_client_local_storage:fetch_device_id(Context) of
                {ok, PersistentNr} ->
                    has_saved_persistent(SurveyId, PersistentNr, Context);
                {error, _} ->
                    false
            end;
        UserId ->
            has_saved_user(SurveyId, UserId, Context)
    end;
has_saved(SurveyId, Context) ->
    has_saved(m_rsc:rid(SurveyId, Context), Context).


has_saved_user(SurveyId, UserId, Context) ->
    z_db:q1("
        select count(*)
        from survey_answers_saved
        where survey_id = $1
          and user_id = $2",
        [SurveyId, UserId],
        Context) == 1.

has_saved_persistent(SurveyId, PersistentNr, Context) ->
    z_db:q1("
        select count(*)
        from survey_answers_saved
        where survey_id = $1
          and persistent = $2",
        [SurveyId, PersistentNr],
        Context) == 1.


%% @doc Delete the intermediate answers for the current user or the persistent
%% id from the user agent.
-spec delete_saved(SurveyId, Context) -> ok when
    SurveyId :: m_rsc:resource() | undefined,
    Context :: z:context().
delete_saved(undefined, _Context) ->
    ok;
delete_saved(SurveyId, Context) when is_integer(SurveyId) ->
    case z_acl:user(Context) of
        undefined ->
            case m_client_local_storage:fetch_device_id(Context) of
                {ok, PersistentNr} ->
                    delete_saved_persistent(SurveyId, PersistentNr, Context);
                {error, _} ->
                    ok
            end;
        UserId ->
            delete_saved_user(SurveyId, UserId, Context)
    end;
delete_saved(SurveyId, Context) ->
    delete_saved(m_rsc:rid(SurveyId, Context), Context).

delete_saved_user(SurveyId, UserId, Context) ->
    z_db:q1("
        delete from survey_answers_saved
        where survey_id = $1
          and user_id = $2",
        [SurveyId, UserId],
        Context),
    ok.

delete_saved_persistent(SurveyId, PersistentNr, Context) ->
    z_db:q1("
        delete from survey_answers_saved
        where survey_id = $1
          and persistent = $2",
        [SurveyId, PersistentNr],
        Context),
    ok.

%% @doc Periodically prune old saved answers. Answers linked to a persistent id
%% are kept for 30 days, answers linked to a user id for 90 days.
-spec prune_saved(Context) -> ok when
    Context :: z:context().
prune_saved(Context) ->
    _ = z_db:q("
        delete from survey_answers_saved
        where modified < now() - interval '30 days'
          and persistent is not null
        ",
        [],
        Context),
    _ = z_db:q("
        delete from survey_answers_saved
        where modified < now() - interval '90 days'
          and user_id is not null
        ",
        [],
        Context),
    ok.
