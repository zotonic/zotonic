%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Check if any answer is wrong.

%% Copyright 2017 Marc Worrell
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

-module(filter_survey_any_wrong_answer).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    survey_any_wrong_answer/3
]).

survey_any_wrong_answer(undefined, _Question, _Context) ->
    false;
survey_any_wrong_answer(Answer, Question, Context) when is_list(Answer) ->
    case z_convert:to_bool(proplists:get_value(is_test, Question, Context)) of
        true ->
            QAnswers = proplists:get_value(answers, Question),
            lists:any(
                fun (Option) ->
                    Val = proplists:get_value(value, Option),
                    IsCorrect = proplists:get_value(is_correct, Option),
                    case {member(Val, Answer), IsCorrect} of
                        {true, false} -> true;
                        {false, true} -> true;
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

