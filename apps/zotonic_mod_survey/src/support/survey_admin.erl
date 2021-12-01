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
        {blk, [{type, Block}]},
        is_new
    ],
    z_render:update(
        <<" #", Element/binary>>,
        #render{template="_admin_survey_question_q.tpl", vars=Vars},
        Context).

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
