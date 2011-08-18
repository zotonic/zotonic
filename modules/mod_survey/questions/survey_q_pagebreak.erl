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
    answer/2,
    prep_answer_header/1,
    prep_answer/2,
    test/3
]).

-include("zotonic.hrl").
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
        {text_label, "Condition"}
    ] ++
    ?QUESTION_AS_PROPLIST(Q).

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


prep_answer_header(_Q) ->
    [].

prep_answer(_Q, _Answer) ->
    [].


%% @doc Evaluate the (optional) jump expression of a pagebreak
test(#survey_question{text=Text, question=Question}, Answers, Context) ->
    case z_string:trim(Text) of
        <<>> -> ok;
        [] -> ok;
        Expr ->
            case z_expression:parse(Expr) of
                {ok, Tree} ->
                    case z_convert:to_bool(
                            z_expression:eval(Tree, 
                                              fun(Var) -> answer_value(Var, Answers) end,
                                              Context))
                    of
                        true -> {jump, Question};
                        false -> ok
                    end;
                Error ->
                    Error
            end
    end.

answer_value(Var, Answers) ->
    proplists:get_value(Var, Answers).

