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

-module(survey_q_likert).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2,
    prep_chart/2,
    prep_answer_header/1,
    prep_answer/2
]).

-include("zotonic.hrl").
-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = likert, 
        name = z_ids:identifier(5),
        text = "", 
        question = <<"Weasels make great pets.">>
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, ""},
        
        {has_question, true},
        {has_text, false},
        {has_name, true},
        
        {question_label, ""},
        {text_label, ""}
    ] ++
    ?QUESTION_AS_PROPLIST(Q).

render(Q) ->
    Name = z_html:escape(Q#survey_question.name),
    Q#survey_question{
        text = "",
        question = iolist_to_binary(Q#survey_question.question),
        html = iolist_to_binary([
            "<p class=\"question\">", z_html:escape(Q#survey_question.question), "</p>",
            "<p class=\"likert\">
                Strongly Disagree
                <input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"1\"/> 1 
                <input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"2\"/> 2
                <input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"3\"/> 3 
                <input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"4\"/> 4 
                <input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"5\"/> 5
                Strongly Agree
            <p>"
            ])
    }.

answer(#survey_question{name=Name}, Answers) ->
    case proplists:get_value(Name, Answers) of
        [C] when C >= $1, C =< $5 -> {ok, [{Name, C - $0}]};
        undefined -> {error, missing}
    end.


prep_chart(_Q, []) ->
    undefined;
prep_chart(Q, [{_, Vals}]) ->
    Labels = [<<"1">>,<<"2">>,<<"3">>,<<"4">>,<<"5">>],
    LabelsDisplay = [<<"Strongly agree">>,<<"Agree">>,<<"Neutral">>,<<"Disagree">>,<<"Strongly disagree">>],

    Values = [ proplists:get_value(C, Vals, 0) || C <- Labels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    [
        {question, z_html:escape(Q#survey_question.question)},
        {values, lists:zip(LabelsDisplay, Values)},
        {type, "pie"},
        {data, [{L,P} || {L,P} <- lists:zip(LabelsDisplay, Perc), P /= 0]}
    ].

prep_answer_header(Q) ->
    z_convert:to_binary(Q#survey_question.name).

prep_answer(_Q, []) ->
    <<>>;
prep_answer(_Q, [{_Name, {Value, _Text}}]) ->
    Value.

