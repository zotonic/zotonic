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

-module(survey_q_truefalse).

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

new() ->
    Q = #survey_question{
        type = truefalse, 
        name = z_ids:identifier(5),
        text = "", 
        question = <<"The earth is flat.">>
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
            "<p class=\"binary\">",
            "<input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"true\">True "
            "<input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"false\">False"
            "<p>"
            ])
    }.

answer(#survey_question{name=Name}, Answers) ->
    case proplists:get_value(Name, Answers) of
        "1" -> {ok, [{Name, true}]};
        "0" -> {ok, [{Name, false}]};
        undefined -> {error, missing}
    end.



prep_chart(_Q, []) ->
    undefined;
prep_chart(Q, [{_, Vals}]) ->
    True = proplists:get_value(<<"true">>, Vals, 0),
    False = proplists:get_value(<<"false">>, Vals, 0),
    Total = True + False,
    TrueP = round(True * 100 / Total),
    FalseP = 100 - TrueP,
    [
     {question, z_html:escape(Q#survey_question.question)},
     {values, [ {Lab,Val} || {Lab,Val} <- [{"true", True}, {"false", False}], Val /= 0 ]},
     {type, "pie"},
     {data, [ [Lab,Val] || [Lab,Val] <- [["true", TrueP], ["false", FalseP]], Val /= 0 ]}
    ].

prep_answer_header(Q) ->
    z_convert:to_binary(Q#survey_question.name).

prep_answer(_Q, []) ->
    <<>>;
prep_answer(_Q, [{_Name, {Value, _Text}}]) ->
    Value.
