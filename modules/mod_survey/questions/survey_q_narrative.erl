-module(survey_q_narrative).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = narrative, 
        name = "",
        question = "",
        text = "I am [age] years old. I like [icecream=vanilla|strawberry|chocolate|other] ice cream and my favorite color is [color      ]."
    },
    render(Q).


question_props(Q) ->
    [
        {explanation, "<p>Type a narrative sentence.<br/>
Use [name ] for an input named \"name\". Pad with spaces to the desired width.<br/>
Use [name=first|second|third] for a drop down menu named \"name\" with the given options.</p>"},
        
        {has_text, true},
        {has_name, false},
        {has_question, false},
        
        {question_label, ""},
        {text_label, "Sentence"},
        
        {type, Q#survey_question.type},
        {name, Q#survey_question.name},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text}
    ].

render(Q) ->
    {Parts, _Inputs} = parse(z_convert:to_list(Q#survey_question.text)),
    Q#survey_question{
        name = "",
        text = iolist_to_binary(Q#survey_question.text),
        question = "",
        parts = Parts,
        html = iolist_to_binary([build(P) || P <- Parts])
    }.


answer(Q, Answers) ->
    {_Html, Inputs} = parse(z_convert:to_list(Q#survey_question.text)),
    answer_inputs(Inputs, Answers, []).

answer_inputs([], _Answers, Acc) ->
    {ok, Acc};
answer_inputs([{IsSelect,Name}|Rest], Answers, Acc) ->
    case proplists:get_value(Name, Answers) of
        undefined -> {error, missing};
        Value -> case z_string:trim(Value) of
                    [] -> {error, missing};
                    V ->
                        V1 = case IsSelect of true -> V; false -> {text, V} end, 
                        answer_inputs(Rest, Answers, [{Name,V1}|Acc])
                 end
    end.


parse(Text) ->
    parse(Text, [], []).

parse([], Acc, InputAcc) ->
    {lists:reverse(Acc), InputAcc};
parse([$[|T], Acc, InputAcc) ->
    {Input1,T1} = lists:splitwith(fun(C) -> C /= $] end, T),
    IsSelect = is_select(Input1),
    Elt = case IsSelect of
        true -> 
            {Name, Options} = split_select(Input1),
            {select, Name, Options};
        false -> 
            Name = z_string:trim(Input1),
            Length = length(Input1),
            {input, Name, Length}
    end,
    Acc1 = [Elt|Acc],
    InputAcc1 = [{IsSelect, Name}|InputAcc],
    case T1 of
        [] -> parse([], Acc1, InputAcc1);
        [$]|T2] -> parse(T2, Acc1, InputAcc1)
    end;
parse(T, Acc, InputAcc) ->
    {Text,Rest} = lists:splitwith(fun(C) -> C /= $[ end, T),
    Text1 = lists:map(fun(10) -> "<br/>";
                         (C) -> C
                      end,
                      z_convert:to_list(z_html:escape(Text))),
    Acc1 = [{html, [], iolist_to_binary(Text1)}|Acc],
    parse(Rest, Acc1, InputAcc).


is_select([]) -> false;
is_select([$=|_]) -> true;
is_select([$||_]) -> true;
is_select([_|T]) -> is_select(T).


build({input, Name, Length}) -> 
    [
        "<input class=\"survey-q inline\" name=\"",z_html:escape(Name),"\" size=\"",integer_to_list(Length),"\" value=\"\" />"
    ];
build({select, Name, Options}) -> 
    [
        "<select class=\"survey-q inline\" name=\"",z_html:escape(Name),"\">",
        options(Options, []),
        "</select>"
    ];
build({html, [], Html}) -> 
    Html.


options([], Acc) ->
    lists:reverse(Acc);
options([""|T], Acc) ->
    Opt = "<option disabled=\"disabled\"></option>",
    options(T, [Opt|Acc]);
options([H|T], Acc) ->
    Opt = [
        "<option>",
        z_html:escape(H),
        "</option>"
    ],
    options(T, [Opt|Acc]).


split_select(I) ->
    {Name, Options} = split_name(I, []),
    Options1 = [ z_string:trim(Opt) || Opt <- string:tokens(Options, "|") ],
    {z_string:trim(Name), Options1}.

split_name([], Acc) ->
    {"", lists:reverse(Acc)};
split_name([$=|Rest], Acc) ->
    {lists:reverse(Acc), Rest};
split_name([H|T], Acc) ->
    split_name(T, [H|Acc]).
