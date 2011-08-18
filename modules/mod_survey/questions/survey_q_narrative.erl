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

-module(survey_q_narrative).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2,
    prep_chart/2,
    prep_answer_header/1,
    prep_answer/2
]).

-include("zotonic.hrl").
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
        {text_label, "Sentence"}
    ] ++
    ?QUESTION_AS_PROPLIST(Q).

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



prep_chart(_Q, []) ->
    undefined;
prep_chart(Q, Answers) ->
    [
        {question, result_title(Q#survey_question.parts, [])},
        {charts, [ prep_chart1(Q, Ans) || Ans <- Answers ]}
    ].

    prep_chart1(Q, {Name, Vals}) ->
        case find_select(Q#survey_question.parts, Name) of
            {select, _, Labels} ->
                LabelsB = [ z_convert:to_binary(Lab) || Lab <- Labels ],
                Values = [ proplists:get_value(C, Vals, 0) || C <- LabelsB ],
                Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
                Perc = [ round(V*100/Sum) || V <- Values ],
                [
                    {values, lists:zip(LabelsB, Values)},
                    {type, "pie"},
                    {data, [{L,P} || {L,P} <- lists:zip(LabelsB, Perc), P /= 0]}
                ];
            _ ->
                undefined
        end.


prep_answer_header(Q) ->
    Parts = [ Part || Part <- Q#survey_question.parts, is_answerable(Part) ],
    [ z_convert:to_binary(Name) || {_, Name, _} <- Parts ].
    
prep_answer(Q, Answers) ->
    Parts = [ Part || Part <- Q#survey_question.parts, is_answerable(Part) ],
    Names = [ z_convert:to_binary(Name) || {_, Name, _} <- Parts ],
    [ z_convert:to_binary(value(proplists:get_value(Name, Answers))) || Name <- Names ].

    is_answerable({html, _, _}) -> false;
    is_answerable(_) -> true.

    value(undefined) -> <<>>;
    value({undefined, X}) -> X;
    value({X, _}) -> X.
    

find_select([], _Name) ->
    undefined;
find_select([{select, Sel, _} = S|Rest], Name) ->
    case z_convert:to_binary(Sel) == Name of
        true -> S;
        false -> find_select(Rest, Name)
    end;
find_select([_|Rest], Name) ->
    find_select(Rest, Name).
    
    
result_title([], Acc) ->
    lists:reverse(Acc);
result_title([{input, _, _}|Parts], Acc) ->
    result_title(Parts, ["…"|Acc]);
result_title([{html, _, Html}|Parts], Acc) ->
    result_title(Parts, [Html|Acc]);
result_title([{select, _, _} = Sel|Parts], Acc) ->
    result_title(Parts, [build(Sel)|Acc]).



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
