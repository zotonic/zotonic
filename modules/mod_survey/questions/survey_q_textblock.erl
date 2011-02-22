-module(survey_q_textblock).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type= textblock, 
        name = z_ids:identifier(5),
        text = "", 
        question = <<"This is a paragraph-length textblock, useful for instructions. If you need more than one paragraph, simply select a textBlock for each paragraph.">>
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, "Please enter the text for the text block."},
        
        {has_question, true},
        {has_text, false},
        {has_name, true},
        
        {question_label, "Text"},
        {text_label, ""},
        
        {type, Q#survey_question.type},
        {name, Q#survey_question.name},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text}
    ].

render(Q) ->
    Q#survey_question{
        text = "",
        question = iolist_to_binary(Q#survey_question.question),
        html = iolist_to_binary(["<p>", z_html:escape(Q#survey_question.question), "</p>"])
    }.

answer(_Q, _Answers) ->
    {ok, none}.

