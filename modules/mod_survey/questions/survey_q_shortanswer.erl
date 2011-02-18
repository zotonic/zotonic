-module(survey_q_shortanswer).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = shortanswer, 
        name = z_ids:identifier(5),
        text = "", 
        question = <<"Please enter your name.">>
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
            "<p class=\"shortanswer\">",
            "<input class=\"survey-q\" type=\"text\" name=\"",Name,"\" value=\"\" /> "
            "<p>"
            ])
    }.

answer(Q, Answers) ->
    Name = Q#survey_question.name,
    case proplists:get_value(Name, Answers) of
        undefined -> {error, missing};
        Value -> case z_string:trim(Value) of
                    [] -> {error, missing};
                    V -> {ok, [{Name, {text, V}}]}
                 end
    end.


