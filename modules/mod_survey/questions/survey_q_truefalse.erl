-module(survey_q_truefalse).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2,
    prep_chart/2
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
prep_chart(_Q, [{_, Vals}]) ->
    True = proplists:get_value(<<"true">>, Vals, 0),
    False = proplists:get_value(<<"false">>, Vals, 0),
    Total = True + False,
    TrueP = round(True * 100 / Total),
    FalseP = 100 - TrueP,
    [
     {values, [{"true", True}, {"false", False}]},
     {type, "pie"},
     {data, [["true", TrueP], ["false", FalseP]]}
    ].
