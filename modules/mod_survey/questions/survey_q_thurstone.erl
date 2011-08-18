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

-module(survey_q_thurstone).

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
        type = thurstone, 
        name = z_ids:identifier(5),
        question = "",
        text = "This editor is very intuitive.
This editor is fairly easy to use.
This editor gets the job done.
This editor is not that easy to use.
This editor is very confusing."
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, ""},
        
        {has_question, true},
        {has_text, true},
        {has_name, true},
        
        {question_label, ""},
        {text_label, "Options"},
        {text_explanation, "<p>Enter the options below, one per line.</p>"}
    ] ++
    ?QUESTION_AS_PROPLIST(Q).

render(Q) ->
    Name = z_html:escape(Q#survey_question.name),
    Options = split_options(Q#survey_question.text),
    Rs = radio(Options, 1, Name, []),
    Q#survey_question{
        question = iolist_to_binary(Q#survey_question.question),
        parts = [ z_html:escape(Opt) || Opt <- Options ],
        html = iolist_to_binary([
            "<p class=\"question\">", z_html:escape(Q#survey_question.question), "</p>",
            "<p class=\"thurstone\">",
            Rs,
            "</p"
            ])
    }.



split_options(Text) ->
    Options = string:tokens(z_string:trim(z_convert:to_list(Text)), "\n"),
    [ z_string:trim(Option) || Option <- Options ].

radio([], _N, _Name, Acc) ->
    lists:reverse(Acc);
radio([""|T], N, Name, Acc) ->
    radio(T, N, Name, Acc);
radio([H|T], N, Name, Acc) ->
    R = ["<input class=\"survey-q\" type=\"radio\" name=\"",Name,"\" value=\"", integer_to_list(N),"\"> ", z_html:escape(H), "<br />"],
    radio(T, N+1, Name, [R|Acc]).


answer(Q, Answers) ->
    Name = Q#survey_question.name,
    Options = split_options(Q#survey_question.text),
    case proplists:get_value(Name, Answers) of
        undefined -> {error, missing};
        N -> {ok, [{Name, lists:nth(list_to_integer(N), Options)}]}
    end.


prep_chart(_Q, []) ->
    undefined;
prep_chart(Q, [{_, Vals}]) ->
    Labels = [ z_convert:to_binary(Lab) || Lab <- split_options(Q#survey_question.text) ],
    Values = [ proplists:get_value(C, Vals, 0) || C <- Labels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    [
        {question, z_html:escape(Q#survey_question.question)},
        {values, lists:zip(Labels, Values)},
        {type, "pie"},
        {data, [{L,P} || {L,P} <- lists:zip(Labels, Perc), P /= 0]}
    ].


prep_answer_header(Q) ->
    z_convert:to_binary(Q#survey_question.name).

prep_answer(_Q, []) ->
    <<>>;
prep_answer(_Q, [{_Name, {Value, _Text}}]) ->
    Value.

    
