-module(survey_q_prompt).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type= prompt, 
        name = "",
        text = "", 
        question = <<"This is a prompt.">>
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, "Please enter the text for the prompt."},
        
        {has_question, true},
        {has_text, false},
        {has_name, false},
        
        {question_label, "Prompt"},
        {text_label, ""},
        
        {type, Q#survey_question.type},
        {name, Q#survey_question.name},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text}
    ].

render(Q) ->
    Q#survey_question{
        name = "",
        text = "",
        question = iolist_to_binary(Q#survey_question.question),
        html = iolist_to_binary(["<p class=\"prompt\">", z_html:escape(Q#survey_question.question), "</p>"])
    }.

answer(_Q, _Answers) ->
    {ok, none}.

