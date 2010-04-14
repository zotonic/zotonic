%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-03-26
%%
%% @doc Model for accessing survey information.

%% Copyright 2010 Marc Worrell
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
    
    insert_survey_submission/3,
    survey_stats/2,
    install/1
]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(questions, #m{value=undefined} = M, _Context) ->
    M#m{value=questions};
m_find_value(Id, #m{value=questions}, Context) ->
    case m_rsc:p(Id, survey, Context) of
        {survey, QuestionIds, Questions} ->
            question_to_value(QuestionIds, Questions, []);
        undefined -> 
            undefined
    end.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, _Context) ->
	[].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.



%% @doc Transform a list of survey questions to template friendly proplists
question_to_value([], _, Acc) ->
    lists:reverse(Acc);
question_to_value([Id|Ids], Qs, Acc) ->
    Q = proplists:get_value(Id, Qs),
    question_to_value(Ids, Qs, [question_to_value1(Id, Q)|Acc]).

    question_to_value1(Id, #survey_question{type=Type, name=Name, html=Html}) ->
        {Id, [{id, Id}, {type, Type}, {name, Name}, {html, Html}]};
    question_to_value1(Id, undefined) ->
        {Id, [{id, Id}, {type, undefined}]}.



%% @doc Save a survey, connect to the current visitor and user (if any)
insert_survey_submission(SurveyId, Answers, Context) ->
    UserId = z_acl:user(Context),
    %% Delete previous answers of this user, if any
    case UserId of
        undefined -> nop;
        _Other -> z_db:q("delete from survey_answer where survey_id = $1 and user_id = $2", [SurveyId, UserId], Context)
    end,
    insert_questions(SurveyId, UserId, Answers, Context).

    insert_questions(_SurveyId, _UserId, [], _Context) ->
        ok;
    insert_questions(SurveyId, UserId, [{QuestionId, Answers}|Rest], Context) ->
        insert_answers(SurveyId, UserId, QuestionId, Answers, Context),
        insert_questions(SurveyId, UserId, Rest, Context).
        
    insert_answers(_SurveyId, _UserId, _QuestionId, [], _Context) ->
        ok;
    insert_answers(SurveyId, UserId, QuestionId, [{Name, Answer}|As], Context) ->
        Args = case Answer of
            {text, Text} -> [SurveyId, UserId, QuestionId, Name, undefined, Text];
            Value -> [SurveyId, UserId, QuestionId, Name, z_convert:to_list(Value), undefined]
        end,
        z_db:q("insert into survey_answer (survey_id, user_id, question, name, value, text) values ($1, $2, $3, $4, $5, $6)", Args, Context),
        insert_answers(SurveyId, UserId, QuestionId, As, Context).


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


%% @doc Install tables used for storing survey results
install(Context) ->
    z_db:ensure_table(survey_answer, [
                #column_def{name=id, type="serial", is_nullable=false},
                #column_def{name=survey_id, type="integer", is_nullable=false},
                #column_def{name=user_id, type="integer", is_nullable=true},
                #column_def{name=question, type="character varying", length=32, is_nullable=false},
                #column_def{name=name, type="character varying", length=32, is_nullable=false},
                #column_def{name=value, type="character varying", length=80, is_nullable=true},
                #column_def{name=text, type="bytea", is_nullable=true}
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

    ok.

