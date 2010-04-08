-module(survey_q_yesno).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type= yesno, 
        name = z_ids:identifier(5),
        text = "", 
        question = <<"Do you like split pea soup?">>
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
            "<p class=\"binary\">",
            "<input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"yes\">Yes "
            "<input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"no\">No"
            "<p>"
            ])
    }.

answer(#survey_question{name=Name}, Context) ->
    case z_context:get_q(Name, Context) of
        "yes" -> {ok, [{Name, yes}]};
        "no" -> {ok, [{Name, no}]};
        undefined -> {error, missing}
    end.
