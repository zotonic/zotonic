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
            Answer = m_survey:single_result(SurveyId, AnswerId, Context),
            Language = case proplists:get_value(language, Answer) of
                undefined ->
                    z_context:language(Context);
                Lang ->
                    {ok, IsoCode} = z_language:to_language_atom(Lang),
                    IsoCode
            end,
            QArgs = z_context:get_q_all_noz(Context),
            {ok, Props} = z_props:from_qs(QArgs),
            Props1 = Props#{
                <<"is_published">> => true,
                <<"category_id">> => cat_person(Context),
                <<"content_group_id">> => cg_person(Context),
                <<"title">> => person_title(Props),
                <<"pref_language">> => Language,
                <<"language">> => [ Language ]
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
    end;
event(#postback{message={admin_show_emails, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    case m_survey:is_allowed_results_download(SurveyId, Context) of
        true ->
            {Headers, Data} = m_survey:survey_results(SurveyId, true, Context),
            All = [lists:zip(Headers, Row) || {_Id,Row} <- Data],
            z_render:dialog(?__("E-mail addresses", Context),
                            "_dialog_survey_email_addresses.tpl",
                            [{id, SurveyId}, {all, All}],
                            Context);
        false ->
            Context
    end;
event(#submit{message={mailinglist_add, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    MailingId = m_rsc:rid(z_context:get_q(<<"mailinglist_id">>, Context), Context),
    case mailinglist_recipients_add(SurveyId, MailingId, Context) of
        {ok, Count} ->
            Msg = iolist_to_binary([
                ?__("Number of recipients added to mailinglist:", Context),
                " ",
                integer_to_binary(Count)
            ]),
            Context1 = z_render:dialog_close(Context),
            z_render:growl(Msg, Context1);
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => mod_mailinglist,
                text => <<"Could not add recipients to mailinglist">>,
                result => error,
                reason => Reason,
                survey_id => SurveyId,
                mailinglist_id => MailingId
            }),
            z_render:growl_error(?__("Could not add the recipients.", Context), Context)
    end;
event(#submit{message={mailinglist_new, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    case m_survey:is_allowed_results_download(SurveyId, Context)
        andalso z_acl:is_allowed(use, mod_mailinglist, Context)
    of
        true ->
            Props = #{
                <<"is_published">> => z_convert:to_bool(z_context:get_q(<<"is_published">>, Context)),
                <<"language">> => [ z_context:language(Context) ],
                <<"category_id">> => mailinglist,
                <<"content_group_id">> => m_rsc:rid(z_context:get_q(<<"content_group_id">>, Context), Context),
                <<"title">> => z_context:get_q(<<"title">>, Context)
            },
            case m_rsc:insert(Props, Context) of
                {ok, MailingId} ->
                    ?LOG_INFO(#{
                        in => mod_mailinglist,
                        text => <<"Mailinglist created for survey email addresses">>,
                        result => ok,
                        category_id => mailinglist,
                        content_group_id => maps:get(<<"content_group_id">>, Props),
                        mailinglist_id => MailingId,
                        survey_id => SurveyId
                    }),
                    case mailinglist_recipients_add(SurveyId, MailingId, Context) of
                        {ok, Count} ->
                            Msg = iolist_to_binary([
                                ?__("Number of recipients added to mailinglist:", Context),
                                " ",
                                integer_to_binary(Count)
                            ]),
                            Context1 = z_render:dialog_close(Context),
                            z_render:growl(Msg, Context1);
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => mod_mailinglist,
                                text => <<"Could not add recipients to mailinglist">>,
                                result => error,
                                reason => Reason,
                                survey_id => SurveyId,
                                mailinglist_id => MailingId
                            }),
                            z_render:growl_error(?__("Could not add the recipients.", Context), Context)
                    end;
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => mod_mailinglist,
                        text => <<"Mailinglist for email addresses could not be created">>,
                        result => error,
                        reason => Reason,
                        category_id => mailinglist,
                        content_group_id => maps:get(<<"content_group_id">>, Props)
                    }),
                    z_render:growl_error(?__("Could not create the mailinglist.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end.

-spec mailinglist_recipients_add(SurveyId, MailingId, Context) -> {ok, CountAdded} | {error, Reason} when
    SurveyId :: m_rsc:resource_id(),
    MailingId :: m_rsc:resource_id(),
    Context :: z:context(),
    CountAdded :: non_neg_integer(),
    Reason :: eacces.
mailinglist_recipients_add(SurveyId, MailingId, Context) ->
    case m_survey:is_allowed_results_download(SurveyId, Context)
        andalso z_acl:is_allowed(use, mod_mailinglist, Context)
        andalso z_acl:rsc_editable(MailingId, Context)
    of
        true ->
            Results = m_survey:list_results(SurveyId, Context),
            Res = lists:map(
                fun(R) ->
                    mailinglist_recipient_add(MailingId, R, Context)
                end,
                Results),
            {ok, lists:sum(Res)};
        false ->
            {error, eacces}
    end.

mailinglist_recipient_add(MailingId, R, Context) ->
    Email = normalize_email(answer_email_address(R)),
    case proplists:get_value(user_id, R) of
        undefined ->
            % No user - add email address directly
            mailinglist_recipient_add_email(MailingId, Email, R, Context);
        UserId ->
            % User - if email is not the same the also add email to mailinglist
            RscAdded = case m_edge:get_id(UserId, subscriberof, MailingId, Context) of
                undefined ->
                    case m_mailinglist:insert_recipient_rsc(MailingId, UserId, Context) of
                        {error, exsubscriberof} ->
                            ?LOG_DEBUG(#{
                                in => zotonic_mod_survey,
                                text => <<"Survey could not add mailinglist recipient">>,
                                result => error,
                                reason => exsubscriberof,
                                mailing_id => MailingId,
                                recipient_id => UserId
                            }),
                            exsubscriberof;
                        {error, Reason} ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_survey,
                                text => <<"Survey could not add mailinglist recipient">>,
                                result => error,
                                reason => Reason,
                                mailing_id => MailingId,
                                recipient_id => UserId
                            }),
                            error;
                        ok ->
                            ok
                    end;
                _ ->
                    false
            end,
            case m_rsc:p_no_acl(UserId, email_raw, Context) of
                Email when RscAdded =:= ok ->
                    1;
                Email when RscAdded =:= exsubscriberof ->
                    0;
                E when E =/= Email, Email =/= <<>>, is_binary(Email) ->
                    % Different Email address or no permission to subscribe user
                    case mailinglist_recipient_add_email(MailingId, Email, R, Context) of
                        1 -> 1;
                        0 when RscAdded =:= ok -> 1;
                        0 -> 0
                    end;
                _ when RscAdded =:= ok ->
                    1;
                _ ->
                    0
            end
    end.

mailinglist_recipient_add_email(_MailingId, undefined, _R, _Context) ->
    0;
mailinglist_recipient_add_email(_MailingId, <<>>, _R, _Context) ->
    0;
mailinglist_recipient_add_email(MailingId, Email, R, Context) ->
    case m_mailinglist:recipient_status(MailingId, Email, Context) of
        subscribed ->
            0;
        unsubscribed ->
            0;
        enoent ->
            Props = [
                {name_first, answer(<<"name_first">>, R)},
                {name_surname_prefix, answer(<<"name_surname_prefix">>, R)},
                {name_surname, answer(<<"name_surname">>, R)},
                {pref_language, proplists:get_value(pref_language, R)}
            ],
            Props1 = drop_undefined(Props),
            case m_mailinglist:insert_recipient(MailingId, Email, Props1, silent, Context) of
                ok ->
                    1;
                {error, _} ->
                    0
            end
    end.

normalize_email(undefined) ->
    undefined;
normalize_email(Email) ->
    m_identity:normalize_key(<<"email">>, Email).

answer_email_address(R) ->
    case answer(<<"email">>, R) of
        undefined ->
            case answer(<<"Email">>, R) of
                undefined ->
                    answer(<<"e-mail">>, R);
                V -> V
            end;
        V -> V
    end.

answer(Prop, R) ->
    case proplists:get_value(props, R) of
        #{ <<"answers">> := As } ->
            case proplists:get_value(Prop, As) of
                undefined ->
                    undefined;
                A when is_list(A) ->
                    case proplists:get_value(answer, A) of
                        V when is_binary(V); is_number(V); is_atom(V) -> V;
                        _ -> undefined
                    end
            end;
        _ ->
            undefined
    end.

drop_undefined(L) ->
    lists:filter(fun({_K,V}) -> V =/= undefined end, L).


person_title(Props) ->
    iolist_to_binary(z_utils:join_defined(<<" ">>, [
        maps:get(<<"name_first">>, Props, undefined),
        maps:get(<<"name_surname_prefix">>, Props, undefined),
        maps:get(<<"name_surname">>, Props, undefined)
    ])).

person_from_answer(Answer, Context) ->
    case proplists:get_value(answers, Answer) of
        undefined ->
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
%% This fixes the page break, page options and stop questions.
%% This combines page jump and page options inputs into serialized blocks.
-spec admin_rscform(QueryArgs) -> QueryArgs1 when
    QueryArgs :: [ QueryArg ],
    QueryArgs1 :: [ QueryArg ],
    QueryArg :: {binary(), term()}.
admin_rscform(Args) ->
    combine_conditions(Args, []).

combine_conditions([], Acc) ->
    lists:flatten(lists:reverse(Acc));
combine_conditions([{<<"page-options-", _Option/binary>>, _} | _] = Bs, Acc) ->
    {Bs1, Block} = combine_page_options(Bs, page_options_block()),
    BlockAsList = [ {<<"blocks[].">>, <<>>} | maps:to_list(Block) ],
    combine_conditions(Bs1, [ BlockAsList | Acc ]);
%% Older editors might have the `is_stop_page` option, newer ones use the `page-options-...`
%% input fields.
combine_conditions([{<<"is_stop_page">>, <<>>}], Acc) ->
    combine_conditions([], Acc);
combine_conditions([{<<"is_stop_page">>, <<>>}, {<<"jump-condition-", _/binary>>, _} = JC|Bs], Acc) ->
    combine_conditions([JC|Bs], Acc);
combine_conditions([{<<"is_stop_page">>, <<>>}|Bs], Acc) ->
    combine_conditions(Bs, [ break_block(<<>>,<<>>,<<>>,<<>>) | Acc ]);
combine_conditions([{<<"is_stop_page">>, _IsChecked}|Bs], Acc) ->
    combine_conditions(Bs, [ stop_block() | Acc ]);
combine_conditions([
            {<<"jump-condition-", _/binary>>, Cond1}, {<<"jump-target-", _/binary>>, Target1},
            {<<"jump-condition-", _/binary>>, Cond2}, {<<"jump-target-", _/binary>>, Target2}|Bs], Acc) ->
    combine_conditions(Bs, [ break_block(Cond1,Target1,Cond2,Target2) | Acc ]);
combine_conditions([
            {<<"jump-condition-", _/binary>>, Cond}, {<<"jump-target-", _/binary>>, Target}|Bs], Acc) ->
    combine_conditions(Bs, [ break_block(Cond,Target,<<>>,<<>>) | Acc ]);
combine_conditions([B|Bs], Acc) ->
    combine_conditions(Bs, [B|Acc]).

%% Combine all page-options into a single block.
combine_page_options([ {<<"page-options-", Option/binary>>, V} | Bs ], Block) ->
    Block1 = Block#{
        <<"blocks[].", Option/binary>> => V
    },
    combine_page_options(Bs, Block1);
combine_page_options(Bs, Block) ->
    {Bs, Block}.

%% @doc This page_options block will be serialized after the page options
%% are fetched.
page_options_block() ->
    #{
        <<"blocks[].type">> => <<"survey_page_options">>,
        <<"blocks[].name">> => z_ids:id(),
        <<"blocks[].is_hide_back">> => false,
        <<"blocks[].is_stop_page">> => false
    }.

%% @doc This stop block is in the serialized form.
stop_block() ->
    [
        {<<"blocks[].">>, <<>>},
        {<<"blocks[].type">>, <<"survey_stop">>},
        {<<"blocks[].name">>, z_ids:id()}
    ].

%% @doc This page_break block is in the serialized form. A page_break block can have
%% one or two conditions/targets. If there are more breaks then more page_break
%% blocks are added.
break_block(Cond1, Target1, Cond2, Target2) ->
    [
        {<<"blocks[].">>, <<>>},
        {<<"blocks[].type">>, <<"survey_page_break">>},
        {<<"blocks[].name">>, z_ids:id()},
        {<<"blocks[].target1">>, Target1},
        {<<"blocks[].condition1">>, Cond1},
        {<<"blocks[].target2">>, Target2},
        {<<"blocks[].condition2">>, Cond2}
    ].
