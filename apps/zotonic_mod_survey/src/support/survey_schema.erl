%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2017 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Schema definition for the survey module.

%% Copyright 2011-2017 Arjan Scherpenisse
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

-module(survey_schema).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").

-export([
    manage_schema/2,

    survey_to_blocks/2,
    results_v1_to_v2/2
]).


%% @doc Install tables used for storing survey results
manage_schema({upgrade, 2}, Context) ->
    % Replace all survey properties with blocks
    {Low, High} = m_category:get_range_by_name(survey, Context),
    Ids = z_db:q("select id from rsc where pivot_category_nr >= $1 and pivot_category_nr <= $2", [Low, High], Context),
    [
        survey_to_blocks(Id, Context) || {Id} <- Ids
    ],
    manage_schema({upgrade, 3}, Context);
manage_schema({upgrade, 3}, Context) ->
    [] = z_db:q("alter table survey_answer add column is_anonymous boolean not null default false", Context),
    manage_schema({upgrade, 4}, Context);
manage_schema({upgrade, 4}, Context) ->
    install_schema_v2(Context),
    ContextAsync = z_context:prune_for_spawn(Context),
    z_proc:spawn_md(
        fun() ->
            ok = upgrade_results_v2(ContextAsync),
            % z_db:q("drop table survey_answer", ContextAsync)
            ok
        end),
    ok;
manage_schema(_Version, Context) ->
    install_schema_v2(Context),
    #datamodel{
        categories=[
            {survey, undefined, [{title, <<"Survey">>}]},
            {poll, survey, [{title, <<"Poll">>}]}
    ]}.


install_schema_v2(Context) ->
    case z_db:table_exists(survey_answers, Context) of
        false ->
            [] = z_db:q("
                create table survey_answers (
                    id bigserial not null,
                    survey_id int not null,
                    user_id int,
                    persistent character varying(32),
                    is_anonymous bool not null default false,
                    language character varying(16),
                    points int not null default 0,
                    props bytea,
                    created timestamp with time zone not null default current_timestamp,
                    modified timestamp with time zone,
                    modifier_id int,

                    constraint survey_answers_pkey primary key (id),
                    constraint fki_survey_answers_survey_id foreign key (survey_id)
                        references rsc (id)
                        on delete cascade
                        on update cascade,
                    constraint fki_survey_answers_user_id foreign key (user_id)
                        references rsc (id)
                        on delete set null
                        on update cascade
                )
                ", Context),
            [] = z_db:q("create index fki_survey_answers_survey_id on survey_answers(survey_id)", Context),
            [] = z_db:q("create index fki_survey_answers_user_id on survey_answers(user_id)", Context),
            [] = z_db:q("create index survey_answers_survey_user_key on survey_answers(survey_id, user_id)", Context),
            [] = z_db:q("create index survey_answers_survey_persistent_key on survey_answers(survey_id, persistent)", Context),
            z_db:flush(Context),
            ok;
        true ->
            case z_db:column_exists(survey_answers, language, Context) of
                false ->
                    z_db:q("
                        alter table survey_answers
                        add column language character varying(16)
                        ", Context),
                    z_db:flush(Context);
                true ->
                    ok
            end,
            ok
    end.

%% @doc Redo the results storage, group individual questions by user/answer
upgrade_results_v2(Context) ->
    SurveyIds = z_db:q("
            select distinct survey_id
            from survey_answer",
            Context),
    lists:foreach(
        fun({Id}) ->
            results_v1_to_v2(Id, Context)
        end,
        SurveyIds).

results_v1_to_v2(Id, Context) ->
    NewResults = v1_survey_results(Id, Context),
    lists:foreach(
        fun(Row) ->
            {ok, _} = z_db:insert(survey_answers, Row, Context)
        end,
        NewResults).


%% @doc Return all results of a survey with separate names, prompts and data
v1_survey_results(SurveyId, Context) ->
    case get_questions(SurveyId, Context) of
        Qs when is_list(Qs) ->
            Rows = z_db:q("select user_id, persistent, is_anonymous, question, name, value, text, created
                           from survey_answer
                           where survey_id = $1
                           order by user_id, persistent", [z_convert:to_integer(SurveyId)], Context),
            Grouped = group_users(Rows),
            Qs1 = map_questions_is_multi(Qs),
            [ v1_row_to_v2(SurveyId, Qs1, UserRow) || UserRow <- Grouped ];
            % IsAnonymous = z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context)),
            % UnSorted = [ user_answer_row(IsAnonymous, User, Created, Answers, NQs, Context) || {User, Created, Answers} <- Grouped ],
            % Sorted = lists:sort(fun([_,_,A|_], [_,_,B|_]) -> A < B end, UnSorted), %% sort by created date
            % Hs = lists:flatten([ <<"user_id">>, <<"anonymous">>, <<"created">>
            %                      | [ answer_header(B, Context) || {_,B} <- NQs ]
            %                    ]),
            % Prompts = lists:flatten([ <<>>, <<>>, <<>>
            %             | [ z_trans:lookup_fallback(answer_prompt(B), Context) || {_,B} <- NQs ]
            %           ]),
            % {Hs, Prompts, Sorted};
        undefined ->
            []
    end.

v1_row_to_v2(SurveyId, Qs, {{user, UserId, Persistent, IsAnonymous}, Created, Answers}) ->
    [
        {survey_id, SurveyId},
        {user_id, UserId},
        {persistent, Persistent},
        {is_anonymous, IsAnonymous},
        {points, 0},
        {created, Created},
        {modified, undefined},
        {modifier_id, undefined},
        {answers, [ v1_answer_to_v2(A, Qs) || A <- Answers ]}
    ].

v1_answer_to_v2({BlockName, {QName, {Value, Text}}}, Qs) ->
    {QName, [
        {block, BlockName},
        {answer, v1_value_to_v2(QName, Value, Text, Qs)}
    ]}.

v1_value_to_v2(QName, Value, Text, Qs) ->
    case proplists:get_value(QName, Qs, false) of
        true ->
            Val = val(Value,Text),
            binary:split(Val, <<"#">>, [global]);
        false ->
            val(Value, Text)
    end.

val(undefined, undefined) -> <<>>;
val(Value, undefined) -> Value;
val(undefined, Text) -> Text;
val(Value, <<>>) -> Value;
val(<<>>, Text) -> Text;
val(Value, _) -> Value.

map_questions_is_multi(Qs) ->
    lists:flatten([ map_question_is_multi(Q) || Q <- Qs ]).

-spec map_question_is_multi({binary(), map()}) -> {binary(), boolean()} | [ {binary(), boolean()} ].
map_question_is_multi({BlockName, Props}) ->
    case z_convert:to_binary(maps:get(<<"type">>, Props, undefined)) of
        <<"survey_narrative">> ->
            Parts = maps:get(<<"parts">>, Props, []),
            QNames = [ z_convert:to_binary(QName) || {_Type, QName, _Opts} <- Parts ],
            [ {QName, false} || QName <- QNames ];
        <<"survey_thurstone">> ->
            {BlockName, true};
        <<"survey_category">> ->
            {BlockName, true};
        <<"survey_", _/binary>> ->
            {BlockName, false};
        _ ->
            []
    end.

get_questions(SurveyId, Context) ->
    case m_rsc:p(SurveyId, blocks, Context) of
        Blocks when is_list(Blocks) ->
            [ {maps:get(<<"name">>, B, undefined), question_prepare(B, Context)} || B <- Blocks];
        _ ->
            undefined
    end.

group_users([]) ->
    [];
group_users([R|Rs]) ->
    {User, Q, Created} = unpack_user_row(R),
    group_users(User, Created, Rs, [Q], []).

group_users(User, Created, [], UserAcc, Acc) ->
    [{User, Created, UserAcc} | Acc];
group_users(User, Created, [R|Rs], UserAcc, Acc) ->
    {U, Q, Crtd} = unpack_user_row(R),
    case User of
        U -> group_users(User, Created, Rs, [Q|UserAcc], Acc);
        _ -> group_users(U, Crtd, Rs, [Q], [{User,Created,UserAcc}|Acc])
    end.

question_prepare(B, Context) ->
    case mod_survey:module_name(maps:get(<<"type">>, B, undefined)) of
        undefined -> B;
        M -> M:prep_block(B, Context)
    end.

unpack_user_row({UserId, Persistent, IsAnonymous, Question, Name, Value, Text, Created}) ->
    {
      {user, UserId, Persistent, IsAnonymous},
      {Question, {Name, {Value, Text}}},
      Created
    }.


% install_survey_answer_table(Context) ->
%     case z_db:table_exists(survey_answer, Context) of
%         false ->
%             z_db:create_table(survey_answer, [
%                         #column_def{name=id, type="serial", is_nullable=false},
%                         #column_def{name=survey_id, type="integer", is_nullable=false},
%                         #column_def{name=user_id, type="integer", is_nullable=true},
%                         #column_def{name=persistent, type="character varying", length=32, is_nullable=true},
%                         #column_def{name=is_anonymous, type="boolean", is_nullable=false, default="false"},
%                         #column_def{name=question, type="character varying", length=32, is_nullable=false},
%                         #column_def{name=name, type="character varying", length=32, is_nullable=false},
%                         #column_def{name=value, type="character varying", length=80, is_nullable=true},
%                         #column_def{name=text, type="bytea", is_nullable=true},
%                         #column_def{name=created, type="timestamp with time zone", is_nullable=true}
%                     ], Context),

%             % Add some indices and foreign keys, ignore errors
%             z_db:equery("create index fki_survey_answer_survey_id on survey_answer(survey_id)", Context),
%             z_db:equery("alter table survey_answer add
%                         constraint fk_survey_answer_survey_id foreign key (survey_id) references rsc(id)
%                         on update cascade on delete cascade", Context),

%             z_db:equery("create index fki_survey_answer_user_id on survey_answer(user_id)", Context),
%             z_db:equery("alter table survey_answer add
%                         constraint fk_survey_answer_user_id foreign key (user_id) references rsc(id)
%                         on update cascade on delete cascade", Context),

%             %% For aggregating answers to survey questions (group by name)
%             z_db:equery("create index survey_answer_survey_name_key on survey_answer(survey_id, name)", Context),
%             z_db:equery("create index survey_answer_survey_question_key on survey_answer(survey_id, question)", Context),
%             z_db:equery("create index survey_answer_survey_user_key on survey_answer(survey_id, user_id)", Context),
%             z_db:equery("create index survey_answer_survey_persistent_key on survey_answer(survey_id, persistent)", Context);

%         true ->
%             ok
%     end.


survey_to_blocks(Id, Context) ->
    case m_rsc:p_no_acl(Id, survey, Context) of
        undefined ->
            ok;
        {survey, QIds, Qs} ->
            QBlocks = [
                question_to_block(proplists:get_value(QId, Qs))
                || QId <- QIds
            ],
            CurrBlocks = to_list(m_rsc:p_no_acl(Id, blocks, Context)),
            m_rsc_update:update(
                Id,
                #{
                    <<"survey">> => undefined,
                    <<"blocks">> => CurrBlocks++QBlocks
                },
                z_acl:sudo(Context)),
            ok
    end.

to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list(L) when is_list(L) -> L.

question_to_block(#survey_question{type=likert} = Q) -> survey_q_likert:to_block(Q);
question_to_block(#survey_question{type=longanswer} = Q) -> survey_q_long_answer:to_block(Q);
question_to_block(#survey_question{type=matching} = Q) -> survey_q_matching:to_block(Q);
question_to_block(#survey_question{type=narrative} = Q) -> survey_q_narrative:to_block(Q);
question_to_block(#survey_question{type=pagebreak} = Q) -> survey_q_page_break:to_block(Q);
question_to_block(#survey_question{type=shortanswer} = Q) -> survey_q_short_answer:to_block(Q);
question_to_block(#survey_question{type=thurstone} = Q) -> survey_q_thurstone:to_block(Q);
question_to_block(#survey_question{type=truefalse} = Q) -> survey_q_truefalse:to_block(Q);
question_to_block(#survey_question{type=yesno} = Q) -> survey_q_yesno:to_block(Q);
question_to_block(#survey_question{type=multiple_choice} = Q) -> survey_q_multiple_choice:to_block(Q);
question_to_block(#survey_question{type=subhead, name=Name, question=Q}) ->
    #{
        <<"type">> => <<"header">>,
        <<"name">> => z_convert:to_binary(Name),
        <<"header">> => z_convert:to_binary(Q)
    };
question_to_block(#survey_question{type=prompt, name=Name, question=Q}) ->
    #{
        <<"type">> =>  <<"text">>,
        <<"name">> =>  z_convert:to_binary(Name),
        <<"body">> =>  z_convert:to_binary(["<p>",Q,"</p>"])
    };
question_to_block(#survey_question{type=textblock, name=Name, question=Q}) ->
    #{
        <<"type">> => <<"text">>,
        <<"name">> => z_convert:to_binary(Name),
        <<"body">> => z_convert:to_binary(["<p>",Q,"</p>"])
    }.

