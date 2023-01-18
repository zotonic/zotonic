%%% @doc Helper functions for editing surveys in the admin.

-module(survey_admin).

-export([
    event/2,
    admin_rscform/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message={insert_block, Args}}, Context) ->
    {id, QId} = proplists:lookup(id, Args),
    Id = m_rsc:rid(QId, Context),
    Block = z_string:to_name( z_context:get_q(<<"block">>, Context) ),
    Element = z_context:get_q(<<"element">>, Context),
    Vars = [
        {element_id, Element},
        {id, Id},
        {is_editable, z_acl:rsc_editable(Id, Context)},
        {edit_language, edit_language(Context)},
        {r_language, r_language(Context)},
        {blk, #{ <<"type">> => Block }},
        is_new
    ],
    z_render:update(
        <<" #", Element/binary>>,
        #render{template="_admin_survey_question_q.tpl", vars=Vars},
        Context);
event(#submit{ message={link_person, Args} }, Context) ->
    {survey_id, SurveyId} = proplists:lookup(survey_id, Args),
    {answer_id, AnswerId} = proplists:lookup(answer_id, Args),
    case z_acl:rsc_editable(SurveyId, Context) of
        true ->
            UserId = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
            m_survey:set_answer_user(SurveyId, AnswerId, UserId, Context),
            z_render:wire({reload, []}, Context);
        false ->
            z_render:growl(
                ?__("Sorry, you are not allowed to do this.", Context),
                Context)
    end;
event(#postback{ message={link_new_person, Args} }, Context) ->
    {survey_id, SurveyId} = proplists:lookup(survey_id, Args),
    {answer_id, AnswerId} = proplists:lookup(answer_id, Args),
    case z_acl:rsc_editable(SurveyId, Context) of
        true ->
            Answer = m_survey:single_result(SurveyId, AnswerId, Context),
            Person = person_from_answer(Answer, Context),
            Vars = #{
                <<"person">> => Person,
                <<"id">> => SurveyId,
                <<"answer_id">> => AnswerId
            },
            z_render:dialog(
                ?__("Link with person", Context),
                "_dialog_survey_link_person_new.tpl",
                Vars,
                Context);
        false ->
            z_render:growl(
                ?__("Sorry, you are not allowed to do this.", Context),
                Context)
    end;
event(#submit{ message={link_person_new, Args} }, Context) ->
    {survey_id, SurveyId} = proplists:lookup(survey_id, Args),
    {answer_id, AnswerId} = proplists:lookup(answer_id, Args),
    case z_acl:rsc_editable(SurveyId, Context) of
        true ->
            QArgs = z_context:get_q_all_noz(Context),
            {ok, Props} = z_props:from_qs(QArgs),
            Props1 = Props#{
                <<"category_id">> => cat_person(Context),
                <<"is_published">> => true,
                <<"content_group_id">> => cg_person(Context)
            },
            case m_rsc:insert(Props1, Context) of
                {ok, PersonId} ->
                    m_survey:set_answer_user(SurveyId, AnswerId, PersonId, Context),
                    z_render:wire({reload, []}, Context);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_survey,
                        text => <<"Survey insert of person failed">>,
                        result => error,
                        reason => Reason,
                        category_id => maps:get(<<"category_id">>, Props1),
                        content_group_id => maps:get(<<"content_group_id">>, Props1)
                    }),
                    z_render:growl_error(
                        ?__("Sorry, could not insert the new person.", Context),
                        Context)
            end;
        false ->
            z_render:growl(
                ?__("Sorry, you are not allowed to do this.", Context),
                Context)
    end.

person_from_answer(undefined, Context) ->
    #{
        <<"category_id">> => cat_person(Context),
        <<"is_published">> => true,
        <<"content_group_id">> => cg_person(Context)
    };
person_from_answer(Answer, Context) ->
    case proplists:get_value(answers, Answer) of
        undefined ->
            #{
                <<"category_id">> => cat_person(Context),
                <<"is_published">> => true,
                <<"content_group_id">> => cg_person(Context)
            };
        [] ->
            #{
                <<"category_id">> => cat_person(Context),
                <<"is_published">> => true,
                <<"content_group_id">> => cg_person(Context)
            };
        Ans ->
            #{
                <<"email">> => ans(Ans, [ <<"email">>, <<"Email">> ]),
                <<"phone">> => ans(Ans, [ <<"phone">>, <<"telephone">>, <<"mobile">>, <<"phone_number">> ]),
                <<"name_first">> => ans(Ans, [ <<"name_first">>, <<"first">>, <<"firstname">>, <<"name">> ]),
                <<"name_surname">> => ans(Ans, [ <<"name_surname">>, <<"surname">>, <<"last">>, <<"lastname">> ]),
                <<"address_country">> => ans(Ans, [ <<"address_country">>, <<"country">> ]),
                <<"address_city">> => ans(Ans, [ <<"address_city">>, <<"city">> ]),
                <<"address_state">> => ans(Ans, [ <<"address_state">>, <<"state">> ]),
                <<"address_street_1">> => ans(Ans, [ <<"address_street_1">>, <<"street_1">>, <<"address_street">>, <<"street">>, <<"street1">> ]),
                <<"address_street_2">> => ans(Ans, [ <<"address_street_2">>, <<"street_2">>, <<"street2">> ]),
                <<"address_postcode">> => ans(Ans, [ <<"address_postcode">>, <<"postcode">>, <<"postal_code">>, <<"zip">>, <<"zipcode">> ])
            }
    end.

cat_person(Context) ->
    case m_rsc:rid(m_config:get_value(mod_survey, person_category, Context), Context) of
        undefined ->
            m_rsc:rid(<<"person">>, Context);
        CatId ->
            CatId
    end.

cg_person(Context) ->
    case m_rsc:rid(m_config:get_value(mod_survey, person_content_group, Context), Context) of
        undefined ->
            m_rsc:rid(<<"default_content_group">>, Context);
        CGId ->
            CGId
    end.

ans(_Ans, []) ->
    undefined;
ans(Ans, [Name|Names]) ->
    case proplists:get_value(Name, Ans) of
        undefined ->
            ans(Ans, Names);
        V ->
            case proplists:get_value(answer, V) of
                B when is_binary(B) ->
                    z_string:trim(B);
                _ ->
                    ans(Ans, Names)
            end
    end.


edit_language(Context) ->
    case z_context:get_q(<<"edit_language">>, Context) of
        undefined ->
            z_context:language(Context);
        <<>> ->
            z_context:language(Context);
        Lang ->
            case z_language:to_language_atom(Lang) of
                {ok, Code} -> Code;
                {error, _} -> z_context:language(Context)
            end
    end.

r_language(Context) ->
    case z_context:get_q(<<"language">>, Context) of
        undefined ->
            [];
        Ls ->
            Ls1 = binary:split(Ls, <<",">>, [global]),
            [ binary_to_atom(L, 'utf8') || L <- lists:filter(fun z_language:is_valid/1, Ls1) ]

    end.


%% @doc Preprocess a posted form before it is given to the rsc update routines.
%%      This fixes the page break and stop questions.
admin_rscform(Args) ->
    combine_conditions(Args, []).

combine_conditions([], Acc) ->
    lists:flatten(lists:reverse(Acc));
combine_conditions([{<<"is_stop_page">>, <<>>}], Acc) ->
    combine_conditions([], Acc);
combine_conditions([{<<"is_stop_page">>, <<>>}, {<<"jump-condition-", _/binary>>, _} = JC|Bs], Acc) ->
    combine_conditions([JC|Bs], Acc);
combine_conditions([{<<"is_stop_page">>, <<>>}|Bs], Acc) ->
    combine_conditions(Bs, break_block(<<>>,<<>>,<<>>,<<>>)++Acc);
combine_conditions([{<<"is_stop_page">>, _IsChecked}|Bs], Acc) ->
    combine_conditions(Bs, stop_block()++Acc);
combine_conditions([
            {<<"jump-condition-", _/binary>>, Cond1}, {<<"jump-target-", _/binary>>, Target1},
            {<<"jump-condition-", _/binary>>, Cond2}, {<<"jump-target-", _/binary>>, Target2}|Bs], Acc) ->
    combine_conditions(Bs, break_block(Cond1,Target1,Cond2,Target2)++Acc);
combine_conditions([
            {<<"jump-condition-", _/binary>>, Cond}, {<<"jump-target-", _/binary>>, Target}|Bs], Acc) ->
    combine_conditions(Bs, break_block(Cond,Target,<<>>,<<>>)++Acc);
combine_conditions([B|Bs], Acc) ->
    combine_conditions(Bs, [B|Acc]).

stop_block() ->
    Id = z_ids:id(),
    [
        {<<"blocks[].type">>, <<"survey_stop">>},
        {<<"blocks[].name">>, Id},
        {<<"blocks[].">>, <<>>}
    ].

break_block(Cond1, Target1, Cond2, Target2) ->
    Id = z_ids:id(),
    [
        {<<"blocks[].target2">>, Target2},
        {<<"blocks[].condition2">>, Cond2},
        {<<"blocks[].target1">>, Target1},
        {<<"blocks[].condition1">>, Cond1},
        {<<"blocks[].type">>, <<"survey_page_break">>},
        {<<"blocks[].name">>, Id},
        {<<"blocks[].">>, <<>>}
    ].
