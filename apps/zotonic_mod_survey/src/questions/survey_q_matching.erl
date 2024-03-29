%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2023 Marc Worrell

%% Copyright 2011-2023 Marc Worrell
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

-module(survey_q_matching).

-export([
    answer/4,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    to_block/1
]).

-include_lib("zotonic_mod_survey/include/survey.hrl").

to_block(Q) ->
    #{
        <<"type">> => <<"survey_matching">>,
        <<"is_required">> => Q#survey_question.is_required,
        <<"name">> => z_convert:to_binary(Q#survey_question.name),
        <<"prompt">> => z_convert:to_binary(Q#survey_question.question),
        <<"matching">> => z_convert:to_binary(Q#survey_question.text)
    }.

-spec answer( m_rsc:resource_id(), map(), list(), z:context() ) -> {ok, list()} | {error, missing}.
answer(_SurveyId, Block, Answers, Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    Props = filter_survey_prepare_matching:survey_prepare_matching(Block, Context),
    Options = [ Val || {Val,_Text} <- maps:get(<<"options">>, Props, []) ],
    Items = maps:get(<<"items">>, Props, []),
    ItemNames = [ iolist_to_binary([Name, $_, KI]) || {KI,_} <- Items ],
    ensure_option(ItemNames, Options, Answers, []).


ensure_option([], _Options, _Answers, Acc) ->
    {ok, Acc};
ensure_option([Name|Ns], Options, Answers, Acc) ->
    case proplists:get_value(Name, Answers) of
        [] ->
            {error, missing};
        undefined ->
            {error, missing};
        Value ->
            case lists:member(Value, Options) of
                true -> ensure_option(Ns, Options, Answers, [{Name,Value}|Acc]);
                false -> {error, missing}
            end
    end.


-spec prep_chart( map(), list(), z:context() ) -> map() | undefined.
prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, Answers, Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    Props = filter_survey_prepare_matching:survey_prepare_matching(Block, Context),
    Items = maps:get(<<"items">>, Props, []),
    ItemNames = [ iolist_to_binary([Name, $_, KI]) || {KI,_} <- Items ],
    Labels = maps:get(<<"options">>, Props, []),
    LabelsB = [ Lab || {Lab, _} <- Labels ],
    #{
        <<"question">> => maps:get(<<"prompt">>, Block, undefined),
        <<"charts">> => [
                    prep_chart1(ItemText, proplists:get_value(ItemName, Answers, []), LabelsB)
                    || {ItemText,ItemName} <- lists:zip(Items, ItemNames)
                 ]
    }.

prep_chart1(_ItemText, undefined, _LabelsB) ->
    undefined;
prep_chart1({_,ItemText}, Vals, LabelsB) ->
    Values = [ proplists:get_value(C, Vals, 0) || C <- LabelsB ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    #{
        <<"question">> => ItemText,
        <<"values">> => lists:zip(LabelsB, Values),
        <<"type">> => <<"pie">>,
        <<"data">> => [{L,P} || {L,P} <- lists:zip(LabelsB, Perc), P /= 0]
    }.


prep_answer_header(Block, _Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    Items = maps:get(<<"items">>, Block, []),
    [ iolist_to_binary([Name, $:, KI]) || {KI,_} <- Items ].


prep_answer(Block, Answers, _Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    Items = maps:get(<<"items">>, Block, []),
    ItemNames = [ iolist_to_binary([Name, $_, KI]) || {KI,_} <- Items ],
    [ prep_answer1(Item, Answers) || Item <- ItemNames ].

prep_answer1(Item, Answers) ->
    case proplists:get_value(Item, Answers) of
        undefined -> <<>>;
        V -> V
    end.

prep_block(Block, Context) ->
    Props = filter_survey_prepare_matching:survey_prepare_matching(Block, Context),
    maps:merge(Block, Props).


