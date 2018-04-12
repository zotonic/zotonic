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
    m_get/2,

    is_allowed_results_download/2,
    get_handlers/1,
    insert_survey_submission/3,
    insert_survey_submission/5,
    replace_survey_submission/4,
    survey_stats/2,
    survey_results/3,
    survey_results_prompts/3,
    survey_results_sorted/3,
    prepare_results/2,

    is_answer_user/2,
    is_answer_user/3,

    list_results/2,
    single_result/3,
    single_result/4,
    delete_result/3,
    delete_result/4,
    delete_results/2,
    get_questions/2,

    rsc_merge/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").



%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ results, Id | Rest ], Context) ->
    {prepare_results(m_rsc:rid(Id, Context), Context), Rest};
m_get([ all_results, [Id, SortColumn] | Rest ], Context) ->
    {survey_results_sorted(m_rsc:rid(Id, Context), SortColumn, Context), Rest};
m_get([ all_results, Id | Rest ], Context) ->
    {survey_results(m_rsc:rid(Id, Context), true, Context), Rest};
m_get([ list_results, Id | Rest ], Context) ->
    {list_results(m_rsc:rid(Id, Context), Context), Rest};
m_get([ get_result, SurveyId, AnswerId | Rest ], Context) ->
    {single_result(SurveyId, AnswerId, Context), Rest};
m_get([ captions, Id | Rest ], Context) ->
    {survey_captions(m_rsc:rid(Id, Context), Context), Rest};
m_get([ totals, Id | Rest ], Context) ->
    {survey_totals(m_rsc:rid(Id, Context), Context), Rest};
m_get([ did_survey, Id | Rest ], Context) ->
    {did_survey(m_rsc:rid(Id, Context), Context), Rest};
m_get([ did_survey_answers, Id | Rest ], Context) ->
    {UserId, PersistentId} = case z_acl:user(Context) of
                                undefined ->
                                    {undefined, persistent_id(Context)};
                                UId ->
                                    {UId, undefined}
                            end,
    As = case m_survey:single_result(m_rsc:rid(Id, Context), UserId, PersistentId, Context) of
        None when None =:= undefined; None =:= [] ->
            [];
        Result ->
            Answers = proplists:get_value(answers, Result, []),
            lists:map(
                fun({_QName, Ans}) ->
                    Block = proplists:get_value(block, Ans),
                    Answer = proplists:get_value(answer, Ans),
                    {Block, Answer}
                end,
                Answers)
    end,
    {As, Rest};
m_get([ did_survey_results, Id | Rest ], Context) ->
    {UserId, PersistentId} = case z_acl:user(Context) of
                                undefined ->
                                    {undefined, persistent_id(Context)};
                                UId ->
                                    {UId, undefined}
                            end,
    {m_survey:single_result(Id, UserId, PersistentId, Context), Rest};
m_get([ did_survey_results_readable, Id | Rest ], Context) ->
    {UserId, PersistentId} = case z_acl:user(Context) of
                                undefined ->
                                    {undefined, persistent_id(Context)};
                                UId ->
                                    {UId, undefined}
                            end,
    SurveyAnswer = m_survey:single_result(m_rsc:rid(Id, Context), UserId, PersistentId, Context),
    {survey_answer_prep:readable_stored_result(Id, SurveyAnswer, Context), Rest};
m_get([ is_allowed_results_download, Id | Rest ], Context) ->
    {is_allowed_results_download(m_rsc:rid(Id, Context), Context), Rest};
m_get([ handlers | Rest ], Context) ->
    {get_handlers(Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.



is_allowed_results_download(Id, Context) ->
    z_acl:rsc_editable(Id, Context)
    orelse z_notifier:first(#survey_is_allowed_results_download{id=Id}, Context) == true.

%% @doc Return the list of known survey handlers
-spec get_handlers(#context{}) -> list({atom(), binary()}).
get_handlers(Context) ->
    z_notifier:foldr(#survey_get_handlers{}, [], Context).


%% @doc Check if the current user/browser did the survey
did_survey(SurveyId, Context) ->
    find_answer_id(SurveyId, z_acl:user(Context), persistent_id(Context), Context) /= undefined.


% persistent_id(#context{session_id = undefined}) -> undefined;
persistent_id(Context) -> z_context:persistent_id(Context).

%% @doc Replace a survey answer
replace_survey_submission(SurveyId, {user, UserId}, Answers, Context) ->
    case z_db:q1("
        select id
        from survey_answers
        where survey_id = $1
          and user_id = $2",
        [SurveyId, UserId],
        Context)
    of
        undefined when is_integer(UserId) ->
            insert_survey_submission(SurveyId, UserId, undefined, Answers, Context);
        AnswerId when is_integer(AnswerId) ->
            replace_survey_submission(SurveyId, AnswerId, Answers, Context)
    end;
replace_survey_submission(SurveyId, AnswerId, Answers, Context) ->
    {Points, AnswersPoints} = survey_test_results:calc_test_results(SurveyId, Answers, Context),
    case z_db:q("
        update survey_answers
        set props = $1,
            points = $2,
            modifier_id = $3,
            modified = now()
        where id = $4
          and survey_id = $5
        ",
        [
            ?DB_PROPS([{answers, AnswersPoints}]),
            Points,
            z_acl:user(Context),
            AnswerId,
            SurveyId
        ],
        Context)
    of
        1 ->
            {UserId,Persistent} = z_db:q_row("select user_id, persistent from survey_answers where id = $1", [AnswerId], Context),
            publish(SurveyId, UserId, Persistent, Context),
            {ok, AnswerId};
        0 ->
            {error, enoent}
    end.

publish(_SurveyId, undefined, _Persistent, _Context) ->
    ok;
publish(SurveyId, UserId, _Persistent, Context) ->
    Topic = iolist_to_binary([
            <<"~site/user/">>,
            z_convert:to_binary(UserId),
            <<"/survey-submission/">>,
            z_convert:to_binary(SurveyId)
        ]),
    z_mqtt:publish(
        Topic,
        [
            {survey_id,SurveyId},
            {user_id, UserId}
        ],
        Context).

%% @doc Save a survey, connect to the current user (if any)
-spec insert_survey_submission(m_rsc:resource_id(), list(), z:context()) -> {ok, pos_integer()} | {error, any()}.
insert_survey_submission(SurveyId, Answers, Context) ->
    case z_acl:user(Context) of
        undefined ->
            insert_survey_submission(SurveyId, undefined, persistent_id(Context), Answers, Context);
        UserId ->
            insert_survey_submission(SurveyId, UserId, undefined, Answers, Context)
    end.

%% @doc Save or replace a survey, resetting the created if needed.
insert_survey_submission(SurveyId, UserId, PersistentId, Answers, Context) ->
    case is_survey_multiple(SurveyId, Context) of
        true ->
            insert_survey_submission_1(SurveyId, UserId, PersistentId, Answers, Context);
        false ->
            % Check if answer exists, if so update the answer
            case find_answer_id(SurveyId, UserId, PersistentId, Context) of
                undefined ->
                    insert_survey_submission_1(SurveyId, UserId, PersistentId, Answers, Context);
                AnsId ->
                    replace_survey_submission(SurveyId, AnsId, Answers, Context)
            end
    end.

is_survey_multiple(SurveyId, Context) ->
    case m_rsc:p_no_acl(SurveyId, survey_multiple, Context) of
        1 -> true;
        <<"1">> -> true;
        _ -> false
    end.

find_answer_id(SurveyId, undefined, PersistentId, Context) ->
    z_db:q1("select id
             from survey_answers
             where survey_id = $1
               and persistent = $2",
            [SurveyId, PersistentId],
            Context);
find_answer_id(SurveyId, UserId, _PersistendId, Context) ->
    z_db:q1("select id
             from survey_answers
             where survey_id = $1
               and user_id = $2",
            [SurveyId, UserId],
            Context).

insert_survey_submission_1(SurveyId, undefined, PersistentId, Answers, Context) ->
    {Points, AnswersPoints} = survey_test_results:calc_test_results(SurveyId, Answers, Context),
    Result = z_db:insert(
        survey_answers,
        [
            {survey_id, SurveyId},
            {user_id, undefined},
            {persistent, PersistentId},
            {is_anonymous, z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context))},
            {points, Points},
            {answers, AnswersPoints}
        ],
        Context),
    publish(SurveyId, undefined, PersistentId, Context),
    Result;
insert_survey_submission_1(SurveyId, UserId, _PersistentId, Answers, Context) ->
    {Points, AnswersPoints} = survey_test_results:calc_test_results(SurveyId, Answers, Context),
    Result = z_db:insert(
        survey_answers,
        [
            {survey_id, SurveyId},
            {user_id, UserId},
            {persistent, undefined},
            {is_anonymous, z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context))},
            {points, Points},
            {answers, AnswersPoints}
        ],
        Context),
    publish(SurveyId, UserId, undefined, Context),
    Result.

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
prepare_result(Block, undefined, _Context) ->
    Type = proplists:get_value(type, Block),
    case mod_survey:module_name(Type) of
        undefined -> {undefined, undefined, Block};
        _ -> {undefined, undefined, undefined}
    end;
prepare_result(Block, Stats, Context) ->
    Type = proplists:get_value(type, Block),
    {
      Stats,
      prep_chart(Type, Block, Stats, Context),
      Block
    }.

%% @private
prep_chart(_Type, _Block, undefined, _Context) ->
    undefined;
prep_chart(Type, Block, Stats, Context) ->
    case mod_survey:module_name(Type) of
        undefined ->
            lager:warning("Not preparing chart for ~p because there is no known handler (~p)",
                         [Type, Stats]),
            undefined;
        M ->
            M:prep_chart(Block, Stats, Context)
    end.


%% @doc Fetch the aggregate answers of a survey.
-spec survey_stats(m_rsc:resource_id(), z:context()) -> 
    list({Block::binary(), [{QName::binary(),[{Answer::binary(),Count::integer()}]}]}).
survey_stats(SurveyId, Context) ->
    Rows = z_db:q("
            select props
            from survey_answers
            where survey_id = $1",
            [SurveyId],
            Context),
    QDict = count_answers(Rows, dict:new()),
    BDict = dict:fold(
        fun({Block,QName,Ans}, Count, Acc) ->
            dict:append({Block,QName}, {Ans,Count}, Acc)
        end,
        dict:new(),
        QDict),
    FinalDict = dict:fold(
        fun({Block,QName}, AnsCt, Acc) ->
            dict:append(Block, {QName,AnsCt}, Acc)
        end,
        dict:new(),
        BDict),
    dict:to_list(FinalDict).

count_answers([], Dict) -> Dict;
count_answers([{Row}|Rows], Dict) ->
    {answers, Answers} = proplists:lookup(answers, Row),
    Dict1 = lists:foldl(
        fun
            ({QName, QAnswer}, Acc) ->
                As = proplists:get_value(answer, QAnswer, []),
                Block = proplists:get_value(block, QAnswer),
                lists:foldl(
                    fun(Ans, QAcc) ->
                        dict:update_counter({Block, QName, Ans}, 1, QAcc)
                    end,
                    Acc,
                    make_list(As));
            (_Error, Acc) ->
                Acc
        end,
        Dict,
        Answers),
    count_answers(Rows, Dict1).

make_list(undefined) -> [];
make_list(L) when is_list(L) -> L;
make_list(V) -> [V].


%% @doc Get survey results, sorted by the given sort column.
survey_results_sorted(SurveyId, SortColumn, Context) ->
    [Headers|Data] = survey_results(SurveyId, true, Context),
    case string:str(Headers, [z_convert:to_binary(SortColumn)]) of
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
-spec get_questions(m_rsc:resource_id(), z:context()) -> [{BlockName::binary(),list()}] | undefined.
get_questions(SurveyId, Context) ->
    case m_rsc:p(SurveyId, blocks, Context) of
        Blocks when is_list(Blocks) ->
            [ {proplists:get_value(name, B), question_prepare(B, Context)} || B <- Blocks];
        _ ->
            undefined
    end.

%% @doc Return all results of a survey
survey_results(SurveyId, IsAnonymous, Context) ->
    {Hs, _Prompts, Data} = survey_results_prompts(SurveyId, IsAnonymous, Context),
    [ Hs | Data ].

%% @doc Return all results of a survey with separate names, prompts and data
survey_results_prompts(undefined, _IsAnonymous, _Context) ->
    {[], [], []};
survey_results_prompts(SurveyId, IsForceAnonymous, Context) when is_integer(SurveyId) ->
    case get_questions(SurveyId, Context) of
        NQs0 when is_list(NQs0) ->
            NQs = drop_hidden_results(NQs0),
            {MaxPoints, PassPercent} = test_pass_values(SurveyId, Context),
            IsAnonymous = IsForceAnonymous orelse z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_anonymous, Context)),
            Rows = z_db:assoc_props("
                        select *
                        from survey_answers
                        where survey_id = $1
                        order by created asc",
                        [SurveyId],
                        Context),
            Answers = [ user_answer_row(Row, NQs, MaxPoints, PassPercent, IsAnonymous, Context) || Row <- Rows ],
            Hs = [ {B, answer_header(B, MaxPoints, Context)} || {_,B} <- NQs ],
            Prompts = [ {B, z_trans:lookup_fallback(answer_prompt(B), Context)} || {_,B} <- NQs ],
            Hs1 = lists:flatten([
                case IsForceAnonymous of
                    true -> [ ?__(<<"Date">>, Context) ];
                    false ->
                        [
                            ?__(<<"Date">>, Context),
                            ?__(<<"User Id">>, Context),
                            ?__(<<"Name">>, Context)
                        ]
                end,
                case MaxPoints of
                    0 -> [];
                    _ -> [
                            ?__(<<"Passed">>, Context),
                            ?__(<<"Score">>, Context),
                            ?__(<<"Percent">>, Context)
                        ]
                end,
                [ H || {_,H} <- Hs ]
            ]),
            Prompts1 = lists:flatten([
                case IsForceAnonymous of
                    true -> [ <<>> ];
                    false -> [ <<>>, <<>>, <<>> ]
                end,
                case MaxPoints of
                    0 -> [];
                    _ -> [ <<>>, <<>>, <<>> ]
                end,
                [ [ P, repeat(<<>>, maybe_length(proplists:get_value(B, Hs, []))-1) ] || {B,P} <- Prompts ]
            ]),
            {Hs1, Prompts1, Answers};
        undefined ->
            {[], [], []}
    end;
survey_results_prompts(SurveyId, IsForceAnonymous, Context) ->
    survey_results_prompts(m_rsc:rid(SurveyId, Context), IsForceAnonymous, Context).

drop_hidden_results(NQs) ->
    lists:filter(
        fun ({_, Block}) ->
            not z_convert:to_bool(proplists:get_value(is_hide_result, Block))
        end,
        NQs).

maybe_length(L) when is_list(L) -> length(L);
maybe_length(_) -> 1.

repeat(_B, N) when N =< 0 -> [];
repeat(B, N) -> [ B | repeat(B,N-1) ].

test_pass_values(SurveyId, Context) ->
    case m_rsc:p_no_acl(SurveyId, survey_test_percentage, Context) of
        undefined -> {0,0};
        <<>> -> {0,0};
        Percentage ->
            {survey_test_results:max_points(SurveyId, Context), z_convert:to_integer(Percentage)}
    end.

user_answer_row(Row, Questions, MaxPoints, PassPercent, IsAnonymous, Context) ->
    Points = proplists:get_value(points, Row),
    Answers = proplists:get_value(answers, Row),
    ByBlock = [
        {proplists:get_value(block, Ans), {Name, Ans}}
        || {Name, Ans} <- Answers
    ],
    {proplists:get_value(id, Row),
     lists:flatten([
        opt_userinfo(IsAnonymous, Row, Context),
        opt_totals(Points, MaxPoints, PassPercent),
        [
            answer_row_question(proplists:get_all_values(QId, ByBlock),
                                Question,
                                MaxPoints > 0,
                                Context)
            || {QId, Question} <- Questions
        ]
     ])}.

opt_userinfo(true, Row, _Context) ->
    [
        proplists:get_value(created, Row)
    ];
opt_userinfo(false, Row, Context) ->
    IsAnonymous = proplists:get_value(is_anonymous, Row),
    UserId = proplists:get_value(user_id, Row),
    case {IsAnonymous, UserId} of
        {true, _} ->
            [
                proplists:get_value(created, Row),
                <<>>,
                <<>>
            ];
        {_, undefined} ->
            [
                proplists:get_value(created, Row),
                <<>>,
                <<>>
            ];
        _ ->
            {Name, _Context} = z_template:render_to_iolist("_name.tpl", [{id, UserId}], z_acl:sudo(Context)),
            [
                proplists:get_value(created, Row),
                UserId,
                iolist_to_binary(Name)
            ]
    end.

opt_totals(_Points, 0, _PassPercent) -> [];
opt_totals(Points, MaxPoints, PassPercent) ->
    Perc = Points/MaxPoints * 100,
    [
        case Perc >= PassPercent of
            true -> <<"+">>;
            false -> <<"-">>
        end,
        z_convert:to_binary(Points),
        z_convert:to_binary(Perc)
    ].

%% @doc private
answer_row_question(_QAnswers, undefined, _IsTest, _Context) ->
    [];
answer_row_question(QAnswers, Q, false, Context) ->
    Type = proplists:get_value(type, Q),
    case mod_survey:module_name(Type) of
        undefined -> [];
        M ->
            Answers = [
                {Name, proplists:get_value(answer, Ans)}
                || {Name,Ans} <- QAnswers
            ],
            M:prep_answer(Q, Answers, Context)
    end;
answer_row_question(QAnswers, Q, true, Context) ->
    case z_convert:to_bool(proplists:get_value(is_test, Q, false)) of
        false ->
            answer_row_question(QAnswers, Q, false, Context);
        true ->
            Type = proplists:get_value(type, Q),
            case mod_survey:module_name(Type) of
                undefined -> [];
                M -> M:prep_answer_score(Q, QAnswers, Context)
            end
    end.

%% @doc private
question_prepare(B, Context) ->
    case mod_survey:module_name(proplists:get_value(type, B)) of
        undefined -> B;
        M -> M:prep_block(B, Context)
    end.

%% @doc private
answer_header(Block, MaxPoints, Context) ->
    Type = proplists:get_value(type, Block),
    case mod_survey:module_name(Type) of
        undefined -> [];
        M ->
            Hs = M:prep_answer_header(Block, Context),
            case z_convert:to_bool(proplists:get_value(is_test, Block)) of
                true when MaxPoints > 0 ->
                    lists:flatten([
                            [H,<<"#">>]
                            || H <- ensure_list(Hs)
                        ]);
                _ ->
                    Hs
            end
    end.

ensure_list(L) when is_list(L) -> L;
ensure_list(V) -> [V].

answer_prompt(Block) ->
    Type = proplists:get_value(type, Block),
    case mod_survey:module_name(Type) of
        undefined -> [];
        _M -> proplists:get_value(prompt, Block, <<>>)
    end.

-spec is_answer_user(integer(), #context{}) -> boolean().
is_answer_user(AnsId, Context) ->
    is_answer_user(AnsId, z_acl:user(Context), Context).

-spec is_answer_user(integer(), integer(), #context{}) -> boolean().
is_answer_user(AnsId, UserId, Context) when is_integer(UserId) ->
    AnsUserId = z_db:q1("
        select user_id
        from survey_answers
        where id = $1",
        [AnsId],
        Context),
    AnsUserId =:= UserId;
is_answer_user(_AnsId, _UserId, _Context) ->
    false.

-spec list_results(integer(), #context{}) -> list().
list_results(SurveyId, Context) when is_integer(SurveyId) ->
    z_db:assoc("
        select *
        from survey_answers
        where survey_id = $1
        order by created
        ",
        [SurveyId],
        Context).

-spec single_result(integer(), integer(), #context{}) -> list().
single_result(SurveyId, AnswerId, Context) when is_integer(SurveyId), is_integer(AnswerId) ->
    case z_db:assoc_props_row("
            select *
            from survey_answers
            where survey_id = $1 and id = $2",
            [SurveyId, AnswerId],
            Context)
    of
        undefined -> [];
        Row -> Row
    end.

%% @doc Retrieve the latest survey result for a user or persistent id.
single_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Arg} = case z_utils:is_empty(UserId) of
                        true -> {"persistent = $2", PersistentId};
                        false -> {"user_id = $2", UserId}
                    end,
    case z_db:assoc_props_row("
            select *
            from survey_answers
            where survey_id = $1
              and "++Clause++"
            order by id desc
            limit 1",
            [ SurveyId, Arg ],
            Context)
    of
        undefined -> [];
        Row -> Row
    end.

%% @doc Delete all survey results
delete_results(SurveyId, Context) ->
    z_db:q("
        DELETE FROM survey_answers
        WHERE id = $2",
        [SurveyId],
        Context).

%% @doc Delete a specific survey results
delete_result(SurveyId, ResultId, Context) ->
    case z_db:q_row("select user_id, persistent from survey_answers where id = $1", [ResultId], Context) of
        {UserId, Persistent} ->
            Result = z_db:q("
                DELETE FROM survey_answers
                WHERE id = $2
                  AND survey_id = $1",
                [SurveyId, ResultId],
                Context),
            publish(SurveyId, UserId, Persistent, Context),
            Result;
        undefined ->
            0
    end.

%% @doc Delete all survey results for a user or persistent id.
delete_result(SurveyId, UserId, PersistentId, Context) ->
    {Clause, Arg} = case z_utils:is_empty(UserId) of
                        true -> {"persistent = $2", PersistentId};
                        false -> {"user_id = $2", UserId}
                    end,
    Result = z_db:q("
        DELETE FROM survey_answers
        WHERE " ++ Clause ++ "
          AND survey_id = $1",
        [SurveyId, Arg],
        Context),
    publish(SurveyId, UserId, PersistentId, Context),
    Result.


%% @doc Move all answers of a to-be-deleted user to another user.
rsc_merge(WinnerId, LoserId, Context) ->
    z_db:q("
        update survey_answers
        set user_id = $1
        where user_id = $2",
        [WinnerId, LoserId],
        Context).

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
            All = lists:map(
                fun(Block) ->
                    Name = proplists:get_value(name, Block),
                    Type = proplists:get_value(type, Block),
                    case mod_survey:module_name(Type) of
                        undefined -> undefined;
                        M ->
                            Value = case proplists:get_value(prep_totals, erlang:get_module_info(M, exports)) of
                                        3 ->
                                            % lager:warning("Name: ~p", [Name]),
                                            Vals = proplists:get_value(Name, Stats),
                                            M:prep_totals(Block, Vals, Context);
                                        undefined ->
                                            undefined
                                    end,
                            {Name, Value}
                    end
                end,
                Blocks),
            AllEmpty = lists:foldl(
                fun(Total, Acc) ->
                    Acc andalso z_utils:is_empty(Total)
                end,
                true,
                All),
            case AllEmpty of
                true -> undefined;
                false -> [ A || A <- All, A =/= undefined ]
            end;
        _ ->
            []
    end.


