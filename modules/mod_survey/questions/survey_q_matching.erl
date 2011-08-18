%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell

%% Copyright 2011 Marc Worrell
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
    new/0,
    question_props/1,
    render/1,
    answer/2,
    prep_chart/2,
    prep_answer_header/1,
    prep_answer/2
]).

-include("../survey.hrl").
-include("zotonic.hrl").

new() ->
    Q = #survey_question{
        type= matching, 
        name = z_ids:identifier(5),
        question = "Match which answer fits best.", 
        text = "apple = red
milk = white
vienna = austria
flying dutchman = wagner
salieri = mozart"
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, "Enter the questions and the matching choices, separated by a '='. You can have more matching choices than questions."},
        
        {has_question, true},
        {has_text, true},
        {has_name, true},
        
        {question_label, ""},
        {text_label, "Matching"},
        {text_explanation, "<p>Enter the matching pairs below, one per line.</p>"}
     ] ++
     ?QUESTION_AS_PROPLIST(Q).

render(Q) ->
    Name = z_html:escape(Q#survey_question.name),
    Pairs = [ split_option(Line) || Line <- split_lines(Q#survey_question.text) ],
    {Qs,As} = lists:unzip(Pairs),
    Qs1 = [ X || X <- Qs, X /= [] ],
    As1 = [ X || X <- As, X /= [] ],
    As2 = z_utils:randomize(As1),
    Select = make_select(As2),
    Q#survey_question{
        text = Q#survey_question.text,
        question = iolist_to_binary(Q#survey_question.question),
        parts = [
            {items, [ z_html:escape(Item) || Item <- Qs1 ] }, 
            {options, [ z_html:escape(Opt) || Opt <- As1 ]} 
        ],
        html = iolist_to_binary(
            [ "<p class=\"question\">", z_html:escape(Q#survey_question.question), "</p",
              "<p class=\"matching\">",
                [ item(X, Select) || X <- Qs1 ],
             "</p>"
            ])
    }.

item(Question, Select) ->
    [
        <<"<span>">>,z_html:escape(Question),<<"</span>">>,
            Select,
        <<"<br/>">>
    ].

make_select(Answers) ->
    [
        <<"<select class=\"survey-q\">">>,
            <<"<option></option>">>,
            [ [<<"<option>">>,z_html:escape(Option),<<"</option>">>] || Option <- Answers ],
        <<"</select>">>
    ].


split_lines(Text) ->
    Options = string:tokens(z_string:trim(z_convert:to_list(Text)), "\n"),
    [ z_string:trim(Option) || Option <- Options ].

split_option(Option) ->
    {Q,M} = lists:splitwith(fun(C) -> C /= $= end, Option),
    {z_string:trim(Q), z_string:trim(drop_eq(M))}.

    drop_eq([$=|Rest]) -> Rest;
    drop_eq(X) -> X.


answer(Q, Answers) ->
    Name = Q#survey_question.name,
    Count = length(proplists:get_value(items, Q#survey_question.parts)),
    Options = proplists:get_value(options, Q#survey_question.parts),
    Names = [ lists:flatten([z_convert:to_list(Name), $_, integer_to_list(N)]) || N <- lists:seq(1,Count) ],
    ensure_option(Names, Options, Answers, []).


ensure_option([], _Options, _Answers, Acc) ->
    {ok, Acc};
ensure_option([Name|Ns], Options, Answers, Acc) ->
    case proplists:get_value(Name, Answers) of
        [] -> {error, missing};
        undefined -> {error, missing};
        Value -> 
            OptVal = z_convert:to_binary(z_html:escape(Value)),
            case lists:member(OptVal, Options) of
                true -> ensure_option(Ns, Options, Answers, [{Name,OptVal}|Acc]);
                false -> {error, missing}
            end
    end.


prep_chart(_Q, []) ->
    undefined;
prep_chart(Q, Answers) ->
    Name = Q#survey_question.name,
    Items = proplists:get_value(items, Q#survey_question.parts),
    ItemNames = [ iolist_to_binary([Name, $_, integer_to_list(N)]) || N <- lists:seq(1,length(Items)) ],
    Labels = proplists:get_value(options, Q#survey_question.parts),
    LabelsB = [ z_convert:to_binary(Lab) || Lab <- Labels ],
    [
        {question, Q#survey_question.question},
        {charts, [ prep_chart1(ItemText, proplists:get_value(ItemName, Answers, []), LabelsB) 
                    || {ItemText,ItemName} <- lists:zip(Items, ItemNames)
                 ]}
    ].

    prep_chart1(_ItemText, undefined, _LabelsB) ->
        undefined;
    prep_chart1(ItemText, Vals, LabelsB) ->
        Values = [ proplists:get_value(C, Vals, 0) || C <- LabelsB ],
        Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
        Perc = [ round(V*100/Sum) || V <- Values ],
        [
            {question, ItemText},
            {values, lists:zip(LabelsB, Values)},
            {type, "pie"},
            {data, [{L,P} || {L,P} <- lists:zip(LabelsB, Perc), P /= 0]}
        ].


prep_answer_header(Q) ->
    Name = Q#survey_question.name,
    Items = proplists:get_value(items, Q#survey_question.parts),
    [ iolist_to_binary([Name, $., integer_to_list(N)]) || N <- lists:seq(1,length(Items)) ].

prep_answer(Q, Answers) ->
    Name = Q#survey_question.name,
    Items = proplists:get_value(items, Q#survey_question.parts),
    ItemNames = [ iolist_to_binary([Name, $., integer_to_list(N)]) || N <- lists:seq(1,length(Items)) ],
    [ prep_answer1(Item, Answers) || Item <- ItemNames ].

    prep_answer1(Item, Answers) ->
        case proplists:get_value(Item, Answers) of
            {V, _Text} -> V;
            undefined -> <<>>
        end.


