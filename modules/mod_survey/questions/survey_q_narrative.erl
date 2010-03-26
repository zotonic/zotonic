-module(survey_q_narrative).

-export([
    new/0,
    question_props/1,
    render/1
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
    Q#survey_question{
        name = "",
        text = iolist_to_binary(Q#survey_question.text),
        question = "",
        html = iolist_to_binary(parse(z_convert:to_list(Q#survey_question.text)))
    }.

parse(Text) ->
    parse(Text, in_text, [], []).

parse([], _State, _Buff, Acc) ->
    lists:reverse(Acc);
parse([$[|T], in_text, [], Acc) ->
    parse(T, in_input, [], Acc);
parse([$]|T], in_input, Input, Acc) ->
    Input1 = lists:reverse(Input),
    Elt = case is_select(Input1) of
        true -> build_select(Input1);
        false -> build_input(Input1)
    end,
    parse(T, in_text, [], [Elt|Acc]);
parse([H|T], in_input, Input, Acc) ->
    parse(T, in_input, [H|Input], Acc);
parse([10|T], in_text, [], Acc) ->
    parse(T, in_text, [], [10,"<br/>"|Acc]);
parse([H|T], in_text, [], Acc) ->
    parse(T, in_text, [], [H|Acc]).


is_select([]) -> false;
is_select([$=|_]) -> true;
is_select([$||_]) -> true;
is_select([_|T]) -> is_select(T).

build_select(I) ->
    {Name, Options} = split_select(I),
    [
        "<select class=\"inline\" name=\"",z_html:escape(Name),"\">",
        options(Options, []),
        "</select>"
    ].

build_input(I) ->
    Name = z_string:trim(I),
    Length = length(I),
    [
        "<input class=\"inline\" name=\"",z_html:escape(Name),"\" size=\"",integer_to_list(Length),"\" value=\"\" />"
    ].

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
