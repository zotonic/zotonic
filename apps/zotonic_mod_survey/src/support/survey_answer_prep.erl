-module(survey_answer_prep).

-export([
    readable/3,
    single/4
]).

readable(Id, Answers0, Context) ->
    Blocks = m_rsc:p(Id, blocks, Context),
    Answers = order_by_blocks(Answers0, Blocks),
    Answers1 = [ {Name, question(Name, Answer, Blocks, Context)} || {Name, Answer} <- Answers ],
    [ {Name, Q} || {Name,Q} <- Answers1, is_list(Q) ].

single(Id, Name, Answer, Context) ->
    Blocks = m_rsc:p(Id, blocks, Context),
    question(Name, Answer, Blocks, Context).


%% @doc Ensure that all answers are in the same order as the blocks
order_by_blocks(As, Bs) ->
    order_by_blocks(As, Bs, []).

order_by_blocks(Rest, [], Acc) ->
    lists:reverse(Acc, Rest);
order_by_blocks(As, [B|Bs], Acc) ->
    Name = proplists:get_value(name, B),
    case proplists:lookup(Name, As) of
        none -> order_by_blocks(As, Bs, Acc);
        Answer -> order_by_blocks(proplists:delete(Name, As), Bs, [Answer|Acc])
    end.

question(Name, Answer, Blocks, Context) ->
    Answer1 = answer_noempty(Answer),
    Block = block(Name, Blocks),
    case z_convert:to_binary(proplists:get_value(type, Block)) of
        <<"survey_country">> ->
            Country = l10n_iso2country:iso2country(Answer),
            [
                {answer_value, z_html:escape_check(Answer)},
                {answer_text, z_html:escape_check(Country)},
                {question, qprops(Block)}
            ];
        <<"survey_", _/binary>> ->
            [
                {answer_value, escape_check(Answer1)},
                {answer_text, escape_check(answer(Answer1, Block, Context))},
                {question, qprops(Block)}
            ];
        _ ->
            undefined
    end.

escape_check({trans, _} = V) ->
    z_html:escape_check(V);
escape_check([[V|_]|_] = L) when not is_integer(V) ->
    [ escape_check(X) || X <- L ];
escape_check([B|_] = L) when is_binary(B); is_list(B); is_tuple(B) ->
    [ z_html:escape_check(X) || X <- L ];
escape_check(V) ->
    z_html:escape_check(V).

qprops(Block) when is_list(Block) ->
    lists:filter(fun keep_qprop/1, Block);
qprops(Block) ->
    Block.

keep_qprop({prompt, _}) -> true;
keep_qprop(_) -> false.

answer_noempty(L) when is_list(L) -> [ A || A <- L, A /= <<>> ];
answer_noempty(A) -> A.

block(Name, []) ->
    % Unknown block, but we have an answer, don't loose the answer.
    [
        {type, <<"survey_short_answer">>},
        {name, z_html:escape_check(Name)},
        {prompt, z_html:escape_check(Name)}
    ];
block(Name, [B|Rest]) ->
    case proplists:get_value(name, B) of
        Name -> B;
        _ -> block(Name, Rest)
    end.

answer(N, Block, Context) ->
    answer_1(proplists:get_value(type, Block), N, Block, Context).

answer_1(<<"survey_thurstone">>, N, Block, Context) ->
    Prep = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, Context),
    Ans = proplists:get_value(answers, Prep),
    Ns = maybe_split(N),
    case is_list(Ns) of
        true -> [ thurs_answer(N1, Ans) || N1 <- Ns ];
        false -> thurs_answer(N, Ans)
    end;
answer_1(<<"survey_yesno">>, N, Block, Context) ->
    case z_convert:to_bool(N) of
        true -> default(proplists:get_value(yes, Block), <<"yes">>, Context);
        false -> default(proplists:get_value(no, Block), <<"no">>, Context)
    end;
answer_1(<<"survey_truefalse">>, N, Block, Context) ->
    case z_convert:to_bool(N) of
        true -> default(proplists:get_value(yes, Block), <<"true">>, Context);
        false -> default(proplists:get_value(no, Block), <<"false">>, Context)
    end;
answer_1(_, N, _, _Context) ->
    N.

thurs_answer(N1, Ans) ->
    proplists:get_value(N1, Ans, N1).

default({trans, _} = Tr, A, Context) ->
    case default(z_trans:lookup_fallback(Tr, Context), xx, Context) of
        xx -> A;
        _ -> Tr
    end;
default(undefined, A, _Context) -> A;
default(<<>>, A, _Context) -> A;
default([], A, _Context) -> A;
default(V, _, _Context) -> V.

maybe_split(B) when is_binary(B) ->
    binary:split(B, <<"#">>, [global]);
maybe_split(V) ->
    V.

