-module(survey_q_likert).

-export([
    new/0,
    question_props/1,
    render/1
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = likert, 
        name = "",
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
        {text_label, ""},
        
        {type, Q#survey_question.type},
        {name, Q#survey_question.name},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text}
    ].

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

