%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc Model for accessing survey information.

%% Copyright 2010-2011 Marc Worrell
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

-module(m_survey).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    is_allowed_results_download/2,
    insert_survey_submission/3,
    survey_stats/2,
    survey_results/2,
    install/1
]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(questions, #m{value=undefined} = M, _Context) ->
    M#m{value=questions};
m_find_value(results, #m{value=undefined} = M, _Context) ->
    M#m{value=results};
m_find_value(did_survey, #m{value=undefined} = M, _Context) ->
    M#m{value=did_survey};
m_find_value(is_allowed_results_download, #m{value=undefined} = M, _Context) ->
    M#m{value=is_allowed_results_download};

m_find_value(Id, #m{value=questions}, Context) ->
    case m_rsc:p(Id, survey, Context) of
        {survey, QuestionIds, Questions} ->
            question_to_value(QuestionIds, Questions, []);
        undefined -> 
            undefined
    end;
m_find_value(Id, #m{value=results}, Context) ->
    prepare_results(Id, Context);
m_find_value(Id, #m{value=did_survey}, Context) ->
    did_survey(Id, Context);
m_find_value(Id, #m{value=is_allowed_results_download}, Context) ->
    is_allowed_results_download(Id, Context).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> list()
m_to_list(#m{value=undefined}, _Context) ->
	[].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.


is_allowed_results_download(Id, Context) ->
    z_acl:rsc_editable(Id, Context)
    orelse z_notifier:first(#survey_is_allowed_results_download{id=Id}, Context) == true.


%% @doc Transform a list of survey questions to admin template friendly proplists
question_to_value([], _, Acc) ->
    lists:reverse(Acc);
question_to_value([Id|Ids], Qs, Acc) ->
    Q = proplists:get_value(Id, Qs),
    question_to_value(Ids, Qs, [question_to_value1(Id, Q)|Acc]).

    question_to_value1(Id, Q) ->
        {Id, [{id, Id} | mod_survey:question_to_props(Q)]}.


%% @doc Check if the current user/browser did the survey
did_survey(SurveyId, Context) ->
    case z_acl:user(Context) of
        undefined ->
            PersistentId = z_context:persistent_id(Context),
            case z_db:q1("select id 
                          from survey_answer
                          where survey_id = $1 
                            and persistent = $2
                          limit 1", [SurveyId, PersistentId], Context) of
                undefined -> false;
                _ -> true
            end;
        UserId ->
            case z_db:q1("select id 
                          from survey_answer
                          where survey_id = $1 
                            and user_id = $2
                          limit 1", [SurveyId, UserId], Context) of
                undefined -> false;
                _ -> true
            end
    end.


%% @doc Save a survey, connect to the current user (if any)
insert_survey_submission(SurveyId, Answers, Context) ->
    UserId = z_acl:user(Context),
    %% Delete previous answers of this user, if any
    PersistentId = case z_convert:to_bool(m_rsc:p(SurveyId, survey_multiple, Context)) of
        true ->
            z_ids:id(30);
        false ->
            case UserId of
                undefined ->
                    PId = z_context:persistent_id(Context),
                    z_db:q("delete from survey_answer where survey_id = $1 and persistent = $2", [SurveyId, PId], Context),
                    PId;
                _Other ->
                    z_db:q("delete from survey_answer where survey_id = $1 and user_id = $2", [SurveyId, UserId], Context),
                    undefined
            end
    end,
    insert_questions(SurveyId, UserId, PersistentId, Answers, Context).

    insert_questions(_SurveyId, _UserId, _PersistentId, [], _Context) ->
        ok;
    insert_questions(SurveyId, UserId, PersistentId, [{QuestionId, Answers}|Rest], Context) ->
        insert_answers(SurveyId, UserId, PersistentId, QuestionId, Answers, Context),
        insert_questions(SurveyId, UserId, PersistentId, Rest, Context).
        
    insert_answers(_SurveyId, _UserId, _PersistentId, _QuestionId, [], _Context) ->
        ok;
    insert_answers(SurveyId, UserId, PersistentId, QuestionId, [{Name, Answer}|As], Context) ->
        Args = case Answer of
            {text, Text} -> [SurveyId, UserId, PersistentId, QuestionId, Name, undefined, Text];
            Value -> [SurveyId, UserId, PersistentId, QuestionId, Name, z_convert:to_list(Value), undefined]
        end,
        z_db:q("insert into survey_answer (survey_id, user_id, persistent, question, name, value, text, created) 
                values ($1, $2, $3, $4, $5, $6, $7, now())", 
               Args,
               Context),
        insert_answers(SurveyId, UserId, PersistentId, QuestionId, As, Context).


prepare_results(SurveyId, Context) ->
    case m_rsc:p(SurveyId, survey, Context) of
        {survey, QuestionIds, Questions} ->
            Stats = survey_stats(SurveyId, Context),
            [
                prepare_result(proplists:get_value(QId, Questions), 
                               proplists:get_value(z_convert:to_binary(QId), Stats)) 
                || QId <- QuestionIds
            ];
        undefined -> 
            undefined
    end.
    
    prepare_result(Question, Stats) ->
        [
         Stats,
         prep_chart(Question, Stats),
         mod_survey:question_to_props(Question)
        ].


prep_chart(_Q, undefined) ->
    undefined;
prep_chart(Q, Stats) ->
    M = mod_survey:module_name(Q),
    M:prep_chart(Q, Stats).



%% @doc Fetch the aggregate answers of a survey, omitting the open text answers. 
%% @spec survey_stats(int(), Context) -> [ {QuestionId, [{Name, [{Value,Count}] }] } ]
survey_stats(SurveyId, Context) ->
    Rows = z_db:q("
                select question, name, value, count(*) 
                from survey_answer 
                where survey_id = $1 and value is not null
                group by question, name, value 
                order by question, name, value", [SurveyId], Context),
    group_questions(Rows, []).
    
    group_questions([], Acc) ->
        lists:reverse(Acc);
    group_questions([{Question,_,_,_}|_] = Answers, Acc) ->
        {Qs,Answers1} = lists:splitwith(fun({Q,_,_,_}) -> Q == Question end, Answers),
        NVs = case Qs of
            [{_, Name, Value, Count}|QsTail] -> group_values(Name, [{Value, Count}], QsTail, []);
            [] -> []
        end,
        group_questions(Answers1, [{Question,NVs}|Acc]).
        
    group_values(Name, Values, [], Acc) ->
        [{Name, Values}|Acc];
    group_values(Name, Values, [{_,Name,V,C}|Rs], Acc) ->
        group_values(Name, [{V,C}|Values], Rs, Acc);
    group_values(Name, Values, [{_,N,V,C}|Rs], Acc) ->
        group_values(N, [{V,C}], Rs, [{Name,Values}|Acc]).


%% @doc Return all results of a survey
survey_results(SurveyId, Context) ->
    case m_rsc:p(SurveyId, survey, Context) of
        {survey, QuestionIds, Questions} ->
             Rows = z_db:q("select user_id, persistent, question, name, value, text, created
                            from survey_answer 
                            where survey_id = $1", [SurveyId], Context),
            Grouped = group_users(Rows),
            QIds = [ z_convert:to_binary(QId) || QId <- QuestionIds ],
            QsB = [ {z_convert:to_binary(QId), Q} || {QId, Q} <- Questions ],
            [
                lists:flatten([ <<"user_id">>, <<"anonymous">>, <<"created">>
                                | [ answer_header(proplists:get_value(QId, Questions)) || QId <- QuestionIds ]
                              ])
                | [ user_answer_row(User, Created, Answers, QIds, QsB, Context) || {User, Created, Answers} <- Grouped ]
            ];
        undefined ->
            []
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


    unpack_user_row({UserId, Persistent, Question, Name, Value, Text, Created}) ->
        {
            {user, UserId, Persistent},
            {Question, {Name, {Value, Text}}},
            Created
        }.


    user_answer_row({user, User, Persistent}, Created, Answers, QuestionIds, Questions, Context) ->
        [
            User,
            Persistent,
            case Created of 
                undefined -> <<>>;
                _ -> erlydtl_dateformat:format(Created, "Y-m-d H:i", Context)
            end
            | answer_row(Answers, QuestionIds, Questions)
        ].
        
    answer_row(Answers, QuestionIds, Questions) ->
        lists:flatten([
            answer_row_question(proplists:get_all_values(QId, Answers), 
                                proplists:get_value(QId, Questions)) || QId <- QuestionIds
        ]).
    
    answer_row_question(_Answer, undefined) ->
        [];
    answer_row_question(Answer, Q) ->
        M = mod_survey:module_name(Q),
        M:prep_answer(Q, Answer).


    answer_header(undefined) ->
        [];
    answer_header(Q) ->
        M = mod_survey:module_name(Q),
        M:prep_answer_header(Q).


%% @doc Install tables used for storing survey results
install(Context) ->
    z_db:ensure_table(survey_answer, [
                #column_def{name=id, type="serial", is_nullable=false},
                #column_def{name=survey_id, type="integer", is_nullable=false},
                #column_def{name=user_id, type="integer", is_nullable=true},
                #column_def{name=persistent, type="character varying", length=32, is_nullable=true},
                #column_def{name=question, type="character varying", length=32, is_nullable=false},
                #column_def{name=name, type="character varying", length=32, is_nullable=false},
                #column_def{name=value, type="character varying", length=80, is_nullable=true},
                #column_def{name=text, type="bytea", is_nullable=true},
                #column_def{name=created, type="timestamp", is_nullable=true}
            ], Context),
            
    % Add some indices and foreign keys, ignore errors
    z_db:equery("create index fki_survey_answer_survey_id on survey_answer(survey_id)", Context),
    z_db:equery("alter table survey_answer add 
                constraint fk_survey_answer_survey_id foreign key (survey_id) references rsc(id) 
                on update cascade on delete cascade", Context),

    z_db:equery("create index fki_survey_answer_user_id on survey_answer(user_id)", Context),
    z_db:equery("alter table survey_answer add 
                constraint fk_survey_answer_user_id foreign key (user_id) references rsc(id) 
                on update cascade on delete cascade", Context),

    %% For aggregating answers to survey questions (group by name)
    z_db:equery("create index survey_answer_survey_name_key on survey_answer(survey_id, name)", Context),
    z_db:equery("create index survey_answer_survey_question_key on survey_answer(survey_id, question)", Context),
    z_db:equery("create index survey_answer_survey_user_key on survey_answer(survey_id, user_id)", Context),
    z_db:equery("create index survey_answer_survey_persistent_key on survey_answer(survey_id, persistent)", Context),

    ok.

