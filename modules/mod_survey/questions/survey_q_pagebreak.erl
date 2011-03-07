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

-module(survey_q_pagebreak).

-export([
    new/0,
    question_props/1,
    render/1,
    answer/2
]).

-include("../survey.hrl").

new() ->
    Q = #survey_question{
        type = pagebreak, 
        name = z_ids:identifier(5), 
        text = "", 
        question = ""
    },
    render(Q).

question_props(Q) ->
    [
        {explanation, "Enter the optional condition for next page."},
        
        {has_question, true},
        {has_text, true},
        {has_name, true},
        
        {question_label, "To question"},
        {text_label, "Condition"},
        
        {type, Q#survey_question.type},
        {name, Q#survey_question.name},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text}
    ].

render(Q) ->
    Q#survey_question{
        question = iolist_to_binary(Q#survey_question.question),
        html = iolist_to_binary([
                    "Pagebreak<br/><hr/>", 
                    case z_html:escape(Q#survey_question.question) of
                        <<>> -> "<em>(no jump)</em>";
                        To -> ["<strong>", To, "</strong>"]
                    end,
                    " if ",
                    case z_html:escape(Q#survey_question.text) of
                        <<>> -> "<em>(no condition)</em>";
                        Cond -> ["<strong>", Cond, "</strong>"]
                    end
                ])
    }.

answer(_Q, _Context) ->
    {ok, none}.


