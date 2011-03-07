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

