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
-export(
   [
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_allowed_results_download/2,
    get_handlers/1,
    insert_survey_submission/3,
    insert_survey_submission/5,
    survey_stats/2,
    survey_results/2,
    survey_results_sorted/3,
    prepare_results/2,
    single_result/4,
    delete_result/4,
    get_questions/2
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
m_find_value(totals, #m{value=undefined} = M, _Context) ->
    M#m{value=totals};
m_find_value(did_survey, #m{value=undefined} = M, _Context) ->
    M#m{value=did_survey};
m_find_value(is_allowed_results_download, #m{value=undefined} = M, _Context) ->
    M#m{value=is_allowed_results_download};
m_find_value(handlers, #m{value=undefined}, Context) ->
    get_handlers(Context);

m_find_value(Id, #m{value=results}, Context) ->
    prepare_results(Id, Context);
m_find_value([Id, SortColumn], #m{value=all_results}, Context) ->
    survey_results_sorted(Id, SortColumn, Context);
m_find_value(Id, #m{value=all_results}, Context) ->
    survey_results(Id, Context);
m_find_value(Id, #m{value=captions}, Context) ->
    survey_captions(Id, Context);
m_find_value(Id, #m{value=totals}, Context) ->
    survey_totals(Id, Context);
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

%% @doc Return the list of known survey handlers
-spec get_handlers(#context{}) -> list({atom(), binary()}).
get_handlers(Context) ->
    z_notifier:foldr(#survey_get_handlers{}, [], Context).


%% @doc Check if the current user/browser did the survey
%% @private
did_survey(SurveyId, Context) ->
    case z_acl:user(Context) of
        undefined ->
            PersistentId = z_context:persistent_id(Context),
            case z_db:q1("select id 
                          from survey_answer
                          where survey_id = $1 
                            and persistent = $2
                          limit 1", [z_convert:to_integer(SurveyId), PersistentId], Context) of
                undefined -> false;
                _ -> true
            end;
        UserId ->
            case z_db:q1("select id 
                          from survey_answer
                          where survey_id = $1 
                            and user_id = $2
                          limit 1", [z_convert:to_integer(SurveyId), UserId], Context) of
                undefined -> false;
                _ -> true
            end
    end.


%% @doc Save a survey, connect to the current user (if any)
insert_survey_submission(SurveyId, Answers, Context) ->
    {UserId, SubmissionId} = case z_convert:to_bool(m_rsc:p(SurveyId, survey_multiple, Context)) of
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
    insert_survey_submission(SurveyId, UserId, SubmissionId, Answers, undefined, Context).

%% @doc Replace a survey for the given userid/persistent_id combination
insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Context) ->
    Created = case UserId of
                  undefined ->
                      z_db:q1("select created from survey_answer where survey_id = $1 and persistent = $2", [z_convert:to_integer(SurveyId), PersistentId], Context);
                  _Other ->
                      z_db:q1("select created from survey_answer where survey_id = $1 and user_id = $2", [z_convert:to_integer(SurveyId), UserId], Context)
              end,
    insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Created, Context).

%% @doc Save or replace a survey, resetting the created if needed.
insert_survey_submission(SurveyId, UserId, PersistentId, Answers, undefined, Context) ->
    insert_survey_submission(SurveyId, UserId, PersistentId, Answers, calendar:universal_time(), Context);
insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Created, Context) ->
    case UserId of
        undefined ->
            z_db:q("delete from survey_answer where survey_id = $1 and persistent = $2", [z_convert:to_integer(SurveyId), PersistentId], Context);
        _Other ->
            z_db:q("delete from survey_answer where survey_id = $1 and user_id = $2", [z_convert:to_integer(SurveyId), UserId], Context)
    end,
    insert_questions(SurveyId, UserId, PersistentId, Answers, Created, Context),
    {ok, PersistentId}.

%% @private
insert_questions(_SurveyId, _UserId, _PersistentId, [], _Created, _Context) ->
    ok;
insert_questions(SurveyId, UserId, PersistentId, [{QuestionId, Answers}|Rest], Created, Context) ->
    insert_answers(SurveyId, UserId, PersistentId, QuestionId, Answers, Created, Context),
    insert_questions(SurveyId, UserId, PersistentId, Rest, Created, Context).

%% @private
insert_answers(_SurveyId, _UserId, _PersistentId, _QuestionId, [], _Created, _Context) ->
    ok;
insert_answers(SurveyId, UserId, PersistentId, QuestionId, [{Name, Answer}|As], Created, Context) ->
    IsAnonymous = z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context)),
    Args = case Answer of
               {text, Text} -> 
                  [SurveyId, UserId, PersistentId, IsAnonymous, QuestionId, Name, undefined, Text, Created];
               Value ->
                  [SurveyId, UserId, PersistentId, IsAnonymous, QuestionId, Name, z_convert:to_list(Value), undefined, Created]
           end,
    z_db:q("insert into survey_answer (survey_id, user_id, persistent, is_anonymous, question, name, value, text, created) 
                values ($1, $2, $3, $4, $5, $6, $7, $8, $9)", 
               Args,
           Context),
    insert_answers(SurveyId, UserId, PersistentId, QuestionId, As, Created, Context).


%% @private
prepare_results(SurveyId, Context) ->
    case m_rsc:p(SurveyId, blocks, Context) of
        [] ->
            undefined;
        <<>> ->
            undefined;
        undefined -> 
            undefined;
        Blocks ->
            Stats = survey_stats(SurveyId, Context),
            [
                begin
                    Name = proplists:get_value(name, Block),
                    prepare_result(Block, proplists:get_value(Name, Stats), Context)
                end
                || Block <- Blocks
            ]
    end.

%% @private
prepare_result(_Block, undefined, _Context) ->
    {undefined, undefined, undefined};
prepare_result(Block, Stats, Context) ->
    Type = proplists:get_value(type, Block),
    {
      Stats,
      prep_chart(Type, Block, Stats, Context),
      [] % mod_survey:question_to_props(Question)
    }.

%% @private
prep_chart(_Type, _Block, undefined, _Context) ->
    undefined;
prep_chart(Type, Block, Stats, Context) ->
    M = mod_survey:module_name(Type),
    M:prep_chart(Block, Stats, Context).



%% @doc Fetch the aggregate answers of a survey. 
%% @spec survey_stats(int(), Context) -> [ {QuestionId, [{Name, [{Value,Count}] }] } ]
survey_stats(SurveyId, Context) ->
    Rows = z_db:q("
                select question, name, value, text
                  from survey_answer 
                  where survey_id = $1
                  order by question, name", [z_convert:to_integer(SurveyId)], Context),
    group_questions(Rows, []).

%% @private
group_questions([], Acc) ->
    lists:reverse(Acc);
group_questions([{Question,_,_,_}|_] = Answers, Acc) ->
    {Qs,Answers1} = lists:splitwith(fun({Q,_,_,_}) -> Q =:= Question end, Answers),
    NVs = group_and_count_values(Qs),
    group_questions(Answers1, [{Question,NVs}|Acc]).


group_and_count_values(Qs) ->
  OnName = split_by_name(Qs),
  count_values(OnName).

split_by_name([]) ->
    [];
split_by_name([{_Q,Name,V,T}|Rest]) ->
    split_by_name_1(Rest, Name, [{V,T}], []).

split_by_name_1([], Name, Vs, Acc) ->
    [{Name,Vs}|Acc];
split_by_name_1([{_Q,Name,V,T}|Rest], Name, Vs, Acc) ->
    split_by_name_1(Rest, Name, [{V,T}|Vs], Acc);
split_by_name_1([{_Q,Name1,V,T}|Rest], Name, Vs, Acc) ->
    split_by_name_1(Rest, Name1, [{V,T}], [{Name,Vs}|Acc]).

count_values(OnName) ->
    lists:foldl(
            fun({Name,Vs},Acc) ->
                Vs1 = split_values(Vs),
                Dict = lists:foldl(
                            fun(V,Acc1) ->
                                dict:update_counter(V, 1, Acc1)
                            end,
                            dict:new(),
                            Vs1),
                [{Name,dict:to_list(Dict)}|Acc]
            end,
            [],
            OnName).

split_values(Vs) ->
    split_values_1(Vs, []).

split_values_1([], Acc) ->
    Acc;
split_values_1([{undefined,undefined}|Vs], Acc) ->
    split_values_1(Vs, Acc);
split_values_1([{V,undefined}|Vs], Acc) ->
    split_values_1(Vs, [V|Acc]);
split_values_1([{undefined,Text}|Vs], Acc) ->
    Ts = binary:split(Text, <<"#">>),
    split_values_1(Vs, Ts++Acc);
split_values_1([{V,Text}|Vs], Acc) ->
    Ts = binary:split(Text, <<"#">>),
    split_values_1(Vs, [V|Ts++Acc]).


%% @doc Get survey results, sorted by the given sort column.
survey_results_sorted(SurveyId, SortColumn, Context) ->
    [Headers|Data] = survey_results(SurveyId, Context),
    case string:str(Headers, [list_to_binary(SortColumn)]) of
        0 ->
            %% column not found, do not sort
            [Headers|Data];
        N ->
            %% Sort on nth row
            Data1 = [{z_string:to_lower(z_convert:to_list(lists:nth(N, Row))), Row} || Row <- Data],
            Data2 = [Row1 || {_, Row1} <- lists:sort(Data1)],
            [Headers|Data2]
    end.



%% @doc get prepared questions from the blocks
-spec get_questions(integer(), #context{}) -> [term()] | undefined.
get_questions(SurveyId, Context) ->
    case m_rsc:p(SurveyId, blocks, Context) of
        Blocks when is_list(Blocks) ->
            [ {proplists:get_value(name, B), question_prepare(B, Context)} || B <- Blocks];
        _ ->
            undefined
    end.


%% @doc Return all results of a survey
survey_results(SurveyId, Context) ->
    case get_questions(SurveyId, Context) of
        NQs when is_list(NQs) ->
            Rows = z_db:q("select user_id, persistent, is_anonymous, question, name, value, text, created
                           from survey_answer 
                           where survey_id = $1
                           order by user_id, persistent", [z_convert:to_integer(SurveyId)], Context),
            Grouped = group_users(Rows),
            IsAnonymous = z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context)), 
            UnSorted = [ user_answer_row(IsAnonymous, User, Created, Answers, NQs, Context) || {User, Created, Answers} <- Grouped ],
            Sorted = lists:sort(fun([_,_,A|_], [_,_,B|_]) -> A < B end, UnSorted), %% sort by created date
            [
                 lists:flatten([ <<"user_id">>, <<"anonymous">>, <<"created">>
                                 | [ answer_header(proplists:get_value(type, B), B, Context) || {_,B} <- NQs ]
                               ])
                 | Sorted
            ];
        undefined ->
            []
    end.
    
%% @doc private
group_users([]) ->
    [];
group_users([R|Rs]) ->
    {User, Q, Created} = unpack_user_row(R),
    group_users(User, Created, Rs, [Q], []).

%% @doc private
group_users(User, Created, [], UserAcc, Acc) ->
    [{User, Created, UserAcc} | Acc];
group_users(User, Created, [R|Rs], UserAcc, Acc) ->
    {U, Q, Crtd} = unpack_user_row(R),
    case User of
        U -> group_users(User, Created, Rs, [Q|UserAcc], Acc);
        _ -> group_users(U, Crtd, Rs, [Q], [{User,Created,UserAcc}|Acc])
    end.

%% @doc private
question_prepare(B, Context) ->
    case mod_survey:module_name(proplists:get_value(type, B)) of
        undefined -> B;
        M -> M:prep_block(B, Context)
    end.

%% @doc private
unpack_user_row({UserId, Persistent, IsAnonymous, Question, Name, Value, Text, Created}) ->
    {
      {user, UserId, Persistent, IsAnonymous},
      {Question, {Name, {Value, Text}}},
      Created
    }.

%% @doc private
user_answer_row(false, {user, User, Persistent, false}, Created, Answers, Questions, Context) ->
    [
        User,
        Persistent
        | user_answer_row_1(Created, Answers, Questions, Context)
    ];
user_answer_row(_IsAnonymous, {user, _User, _Persistent, _UserAnonymous}, Created, Answers, Questions, Context) ->
    [
        undefined,
        undefined
        | user_answer_row_1(Created, Answers, Questions, Context)
    ].

user_answer_row_1(Created, Answers, Questions, Context) ->
    [
        case Created of 
           undefined -> <<>>;
           _ -> erlydtl_dateformat:format(Created, "Y-m-d H:i", Context)
        end
        | answer_row(Answers, Questions, Created, Context)
    ].

%% @doc private
answer_row(Answers, Questions, Created, Context) ->
    Answers1 = case Created =:= undefined orelse Created >= {{2012,12,1},{0,0,0}} of
                   true ->
                       Answers;
                   false ->
                       %% Convert answers to proper format if old format is discovered and survey was filled before 2012-12-01
                       [case A of
                            {X, {X, _}} -> A;
                            {_, {X, Y}} -> {X, {X, Y}}
                        end || A <- Answers]
               end,
    lists:flatten([
                   answer_row_question(proplists:get_all_values(QId, Answers1), 
                                       Question,
                                       Context)
                   || {QId, Question} <- Questions
                  ]).

%% @doc private
answer_row_question(_Answer, undefined, _Context) ->
    [];
answer_row_question(Answer, Q, Context) ->
    Type = proplists:get_value(type, Q),
    case mod_survey:module_name(Type) of
        undefined -> [];
        M -> M:prep_answer(Q, Answer, Context)
    end.

%% @doc private
answer_header(Type, Block, Context) ->
    case mod_survey:module_name(Type) of
        undefined -> [];
        M -> M:prep_answer_header(Block, Context)
    end.


%% @doc Retrieve a single survey result for a user or persistent id.
single_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Args} = case z_utils:is_empty(UserId) of
                         true -> {"persistent = $1", [PersistentId]};
                         false -> {"user_id = $1", [UserId]}
                     end,
    Rows = z_db:q("SELECT question, name, value, text FROM survey_answer WHERE " ++ Clause ++ "AND survey_id = $2", Args ++ [z_convert:to_integer(SurveyId)], Context),
    lists:foldr(fun({QId, Name, Numeric, Text}, R) ->
                        Value = case z_utils:is_empty(Text) of
                                    true -> Numeric; 
                                    false -> Text
                                end,
                        z_utils:prop_replace(QId, z_utils:prop_replace(Name, Value, proplists:get_value(QId, R, [])), R)
                end,
                [],
                Rows).


%% @doc Delete a survey result for a user or persistent id.
delete_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Args} = case z_utils:is_empty(UserId) of
                         true -> {"persistent = $1", [PersistentId]};
                         false -> {"user_id = $1", [UserId]}
                     end,
    z_db:q("DELETE FROM survey_answer WHERE " ++ Clause ++ " and survey_id = $2", Args ++ [z_convert:to_integer(SurveyId)], Context).


%% @private
survey_captions(Id, Context) ->
    case m_rsc:p(Id, blocks, Context) of
        Blocks when is_list(Blocks) ->
            [
                {<<"created">>, ?__("Created", Context)} |
                [ {proplists:get_value(name, Block), proplists:get_value(prompt, Block)} || Block <- Blocks ]
            ];
        _ ->
            []
    end.


%% @private
survey_totals(Id, Context) ->
    Stats = survey_stats(Id, Context),
    case m_rsc:p(Id, blocks, Context) of
        Blocks when is_list(Blocks) ->
            All = lists:map(fun(Block) ->
                                    Name = proplists:get_value(name, Block),
                                    Type = proplists:get_value(type, Block),
                                    M = mod_survey:module_name(Type),
                                    Value = case proplists:get_value(prep_totals, erlang:get_module_info(M, exports)) of
                                                3 ->
                                                    lager:warning("Name: ~p", [Name]),
                                                    Vals = proplists:get_value(Name, Stats),
                                                    M:prep_totals(Block, Vals, Context);
                                                undefined ->
                                                    undefined
                                            end,
                                    {Name, Value}
                            end,
                            Blocks),
            AllEmpty = lists:foldl(fun(Total, Acc) -> z_utils:is_empty(Total) and Acc end, true, All),
            case AllEmpty of
                true ->
                    undefined;
                false ->
                    All
            end;
        _ ->
            []
    end.

                
