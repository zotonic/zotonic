-module(survey_q_subhead).

-export([
    new/0,
    question_props/1,
    render/1
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = subhead, 
        name = "", 
        text = "", 
        question = <<"This is a subhead">>
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, "Please enter the text for the subhead."},
        
        {has_question, true},
        {has_text, false},
        {has_name, false},
        
        {question_label, "Subhead"},
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
        html = iolist_to_binary(["<h2>", z_html:escape(Q#survey_question.question), "</h2>"])
    }.

