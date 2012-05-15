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
    insert_survey_submission/5,
    survey_stats/2,
    survey_results/2,
	single_result/4,
	delete_result/4
]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(questions, #m{value=undefined} = M, _Context) ->
    M#m{value=questions};
m_find_value(results, #m{value=undefined} = M, _Context) ->
    M#m{value=results};
m_find_value(all_results, #m{value=undefined} = M, _Context) ->
    M#m{value=all_results};
m_find_value(captions, #m{value=undefined} = M, _Context) ->
    M#m{value=captions};
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
m_find_value([Id, SortColumn], #m{value=all_results}, Context) ->
    survey_results_sorted(Id, SortColumn, Context);
m_find_value(Id, #m{value=all_results}, Context) ->
    survey_results(Id, Context);
m_find_value(Id, #m{value=captions}, Context) ->
    survey_captions(Id, Context);
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
    {UserId, PersistentId} = case z_convert:to_bool(m_rsc:p(SurveyId, survey_multiple, Context)) of
                                 true ->
                                     {undefined, z_ids:id(30)};
                                 false ->
                                     case z_acl:user(Context) of
                                         undefined ->
                                             {undefined, z_context:persistent_id(Context)};
                                         U ->
                                             {U, undefined}
                                     end
                             end,
    insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Context).

%% @doc Save or replace a survey for the given userid/persistent_id combination
insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Context) ->
    case UserId of
        undefined ->
            PId = z_context:persistent_id(Context),
            z_db:q("delete from survey_answer where survey_id = $1 and persistent = $2", [SurveyId, PId], Context);
        _Other ->
            z_db:q("delete from survey_answer where survey_id = $1 and user_id = $2", [SurveyId, UserId], Context)
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


survey_results_sorted(SurveyId, SortColumn, Context) ->
    [Headers|Data] = survey_results(SurveyId, Context),
    case string:str(Headers, [list_to_binary(SortColumn)]) of
        0 ->
            %% column not found, do not sort
            [Headers|Data];
        N ->
            %% Sort on nth row
            Data1 = [{lists:nth(N, Row), Row} || Row <- Data],
            Data2 = [Row1 || {_, Row1} <- lists:sort(Data1)],
            [Headers|Data2]
    end.

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
            UnSorted = [ user_answer_row(User, Created, Answers, QIds, QsB, Context) || {User, Created, Answers} <- Grouped ],
            Sorted = lists:sort(fun([_,_,A|_], [_,_,B|_]) -> A < B end, UnSorted), %% sort by created date
            [
             lists:flatten([ <<"user_id">>, <<"anonymous">>, <<"created">>
                             | [ answer_header(proplists:get_value(QId, Questions)) || QId <- QuestionIds ]
                           ])
             | Sorted];
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


single_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Args} = case z_utils:is_empty(UserId) of
                         true -> {"persistent = $1", [PersistentId]};
                         false -> {"user_id = $1", [UserId]}
                     end,
    Rows = z_db:q("SELECT question, name, value, text FROM survey_answer WHERE " ++ Clause ++ "AND survey_id = $2", Args ++ [SurveyId], Context),
    lists:foldr(fun({Q, N, Numeric, Text}, R) ->
                        QId = z_convert:to_list(Q),
                        Name = z_convert:to_list(N),
                        Value = case z_utils:is_empty(Text) of
                                    true -> Numeric; false -> {text, z_convert:to_list(Text)}
                                end,
                        z_utils:prop_replace(QId, z_utils:prop_replace(Name, Value, proplists:get_value(QId, R, [])), R)
                end,
                [],
                Rows).


delete_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Args} = case z_utils:is_empty(UserId) of
                         true -> {"persistent = $1", [PersistentId]};
                         false -> {"user_id = $1", [UserId]}
                     end,
    z_db:q("DELETE FROM survey_answer WHERE " ++ Clause ++ " and survey_id = $2", Args ++ [SurveyId], Context).


survey_captions(Id, Context) ->
    {survey, _, Questions} = m_rsc:p(Id, survey, Context),
    [{<<"created">>, ?__("Created", Context)} |
     [
      {list_to_binary(S#survey_question.name),
       S#survey_question.question}
      || {_, S} <- Questions]
    ].
