%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2022 Marc Worrell
%% @doc Check if any answer is correct.

%% Copyright 2017-2022 Marc Worrell
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

-module(filter_survey_any_correct_answer).

-export([
    survey_any_correct_answer/3
]).

survey_any_correct_answer(undefined, _Question, _Context) ->
    false;
survey_any_correct_answer(Answer, Question, _Context) when is_list(Answer) ->
    case z_convert:to_bool(maps:get(<<"is_test">>, Question, false)) of
        true ->
            QAnswers = maps:get(<<"answers">>, Question, []),
            lists:any(
                fun (Option) ->
                    Val = maps:get(<<"value">>, Option, undefined),
                    IsCorrect = maps:get(<<"is_correct">>, Option, false),
                    case {member(Val, Answer), IsCorrect} of
                        {true, true} -> true;
                        _ -> false
                    end
                end,
                QAnswers);
        false ->
            false
    end.

member(A, A) -> true;
member(A, L) when is_list(L) -> lists:member(A, L);
member(_, _) -> false.

