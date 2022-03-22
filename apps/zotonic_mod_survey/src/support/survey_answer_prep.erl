%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2022 Marc Worrell

%% Copyright 2017-2022 Marc Worrell
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

-module(survey_answer_prep).

-export([
    readable_stored_result/3,
    readable/3,
    single/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Used to format results as fetched from the database
readable_stored_result(Id, SurveyAnswer, Context) ->
    case proplists:get_value(answers, SurveyAnswer, []) of
        [] -> [];
        Answers ->
            QVs = lists:foldl(
                fun({QName, Ans}, Acc) ->
                    Vs = proplists:get_value(answer, Ans),
                    [{QName,Vs} | Acc]
                end,
                [],
                Answers),
            readable(Id, QVs, Context)
    end.

%% @doc Used to format results as stored during the survey
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
    Name = maps:get(<<"name">>, B),
    case proplists:lookup(Name, As) of
        none -> order_by_blocks(As, Bs, Acc);
        Answer -> order_by_blocks(proplists:delete(Name, As), Bs, [Answer|Acc])
    end.

question(Name, Answer, Blocks, Context) ->
    Answer1 = answer_noempty(Answer),
    Block = block(Name, Blocks),
    Type = z_convert:to_binary(maps:get(<<"type">>, Block)),
    question_1(Type, Answer1, Block, Context).

question_1(<<"survey_country">>, Answer, Block, _Context) ->
    Country = l10n_iso2country:iso2country(Answer),
    [
        {answers, [
            [
                {value, escape_check(Answer)},
                {text, escape_check(Country)}
            ]
        ]},
        % {answer_value, z_html:escape_check(Answer)},
        % {answer_text, z_html:escape_check(Country)},
        {question, qprops(Block)}
    ];
question_1(<<"survey_yesno">>, Answer, Block, Context) ->
    Text = case z_convert:to_bool(Answer) of
        true -> default(maps:get(<<"yes">>, Block, undefined), <<"yes">>, Context);
        false -> default(maps:get(<<"no">>, Block, undefined), <<"no">>, Context)
    end,
    [
        {answers, [
            [
                {value, escape_check(Answer)},
                {text, escape_check(Text)}
            ]
        ]},
        % {answer_value, escape_check(Answer)},
        % {answer_text, escape_check(Text)},
        {question, qprops(Block)}
    ];
question_1(<<"survey_truefalse">>, Answer, Block, Context) ->
    Text = case z_convert:to_bool(Answer) of
        true -> default(maps:get(<<"yes">>, Block, undefined), <<"true">>, Context);
        false -> default(maps:get(<<"no">>, Block, undefined), <<"false">>, Context)
    end,
    [
        {answers, [
            [
                {value, escape_check(Answer)},
                {text, escape_check(Text)}
            ]
        ]},
        % {answer_value, escape_check(Answer)},
        % {answer_text, escape_check(Text)},
        {question, qprops(Block)}
    ];
question_1(<<"survey_thurstone">>, Answer, Block, Context) ->
    Block1 = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
    Ans = maps:get(<<"answers">>, Block1, undefined),
    Ns = maybe_split(Answer),
    Answers = case is_list(Ns) of
        true -> [ thurs_answer(N1, Ans) || N1 <- Ns ];
        false -> [ thurs_answer(Answer, Ans) ]
    end,
    [
        {answers, Answers},
        % {answer_value, escape_check(Answer)},
        % {answer_text, escape_check(Answer)},
        {question, qprops(Block)}
    ];
question_1(<<"survey_", _/binary>>, Answer, Block, _Context) ->
    [
        {answer_value, escape_check(Answer)},
        {answer_text, escape_check(Answer)},
        {question, qprops(Block)}
    ];
question_1(_Type, _Answer, _Block, _Context) ->
    undefined.


escape_check({trans, _} = V) ->
    z_html:escape_check(V);
escape_check([[V|_]|_] = L) when not is_integer(V) ->
    [ escape_check(X) || X <- L ];
escape_check([B|_] = L) when is_binary(B); is_list(B); is_tuple(B) ->
    [ z_html:escape_check(X) || X <- L ];
escape_check(V) ->
    z_html:escape_check(V).

qprops(Block) when is_map(Block) ->
    maps:filter(fun keep_qprop/2, Block).

keep_qprop(<<"prompt">>, _) -> true;
keep_qprop(_, _) -> false.

answer_noempty(L) when is_list(L) -> [ A || A <- L, A /= <<>> ];
answer_noempty(A) -> A.

block(Name, []) ->
    % Unknown block, but we have an answer, don't loose the answer.
    #{
        <<"type">> => <<"survey_short_answer">>,
        <<"name">> => z_html:escape_check(Name),
        <<"prompt">> => z_html:escape_check(Name)
    };
block(Name, [B|Rest]) ->
    case maps:get(<<"name">>, B, undefined) of
        Name -> B;
        _ -> block(Name, Rest)
    end.

thurs_answer(V, []) ->
    [
        {value, escape_check(V)},
        {text, escape_check(V)}
    ];
thurs_answer(V, [#{ <<"value">> := V } = A | _ ]) ->
    [
        {value, escape_check(V)},
        {text, escape_check(maps:get(<<"option">>, A))},
        {is_correct, maps:get(<<"is_correct">>, A, undefined)},
        {feedback, maps:get(<<"feedback">>, A, undefined)}
    ];
thurs_answer(V, [ _ | As ]) ->
    thurs_answer(V, As).

default(#trans{} = Tr, A, Context) ->
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

