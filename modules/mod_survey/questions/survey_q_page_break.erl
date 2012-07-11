%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2012 Marc Worrell

%% Copyright 2011-2012 Marc Worrell
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

-module(survey_q_page_break).

-export([
    answer/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    test/3,
    to_block/1
]).

-include("../survey.hrl").

to_block(Q) ->
    [
        {type, survey_page_break},
        {name, z_convert:to_binary(Q#survey_question.name)},
        {condition1, z_convert:to_binary(Q#survey_question.text)},
        {target1, z_convert:to_binary(Q#survey_question.question)}
    ].


answer(_Block, _Answers, _Context) ->
    {ok, none}.

prep_answer_header(_Q, _Context) ->
    [].

prep_answer(_Q, _Answer, _Context) ->
    [].

prep_block(B, _Context) ->
    B.


%% @doc Evaluate the (optional) jump expression of a page break
test(Q, Answers, Context) ->
    case eval(proplists:get_value(condition1, Q), proplists:get_value(target1, Q), Answers, Context) of
        {jump, _} = Jump1 -> Jump1;
        _ -> eval(proplists:get_value(condition2, Q), proplists:get_value(target2, Q), Answers, Context)
    end.
    
eval(undefined, _, _, _Context) ->
    ok;
eval(_, undefined, _, _Context) ->
    ok;
eval(Expr, Target, Answers, Context) ->
    eval1(z_string:trim(Expr), z_string:trim(Target), Answers, Context).

eval1(<<>>, _, _, _Context) ->
    ok;
eval1(_, <<>>, _, _Context) ->
    ok;
eval1(Expr, Target, Answers, Context) ->
    case z_expression:parse(Expr) of
        {ok, Tree} ->
            case z_convert:to_bool(
                    z_expression:eval(Tree, 
                                      fun(Var) ->
                                          proplists:get_value(Var, Answers)
                                      end,
                                      Context))
            of
                true -> {jump, Target};
                false -> ok
            end;
        Error ->
            Error
    end.

