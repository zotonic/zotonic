%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2023 Marc Worrell

%% Copyright 2011-2023 Marc Worrell
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

-module(survey_q_short_answer).

-export([
    answer/4,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    prep_totals/3,
    to_block/1
]).

-include_lib("zotonic_mod_survey/include/survey.hrl").

answer(_SurveyId, Block, Answers, _Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    case proplists:get_value(Name, Answers) of
        undefined ->
            {error, missing};
        Value ->
            case z_string:trim(Value) of
                <<>> -> {error, missing};
                V -> {ok, [{Name, V}]}
            end
    end.

prep_chart(_Block, _Ans, _Context) ->
    undefined.

prep_answer_header(Q, _Context) ->
    maps:get(<<"name">>, Q, undefined).

prep_answer(_Q, [], _Context) ->
    <<>>;
prep_answer(_Q, [{_Name, Ans}|_], _Context) ->
    z_convert:to_binary(Ans).

prep_block(B, _Context) ->
    B.



to_block(Q) ->
    #{
        <<"type">> => <<"survey_short_answer">>,
        <<"is_required">> => Q#survey_question.is_required,
        <<"name">> => z_convert:to_binary(Q#survey_question.name),
        <<"prompt">> => z_convert:to_binary(Q#survey_question.question)
    }.


prep_totals(#{ <<"validation">> := <<"numericality">> }, [ {_, Vals} ], _) ->
    lists:foldl(
      fun({K, V}, Sum) ->
              try
                  (z_convert:to_integer(K) * z_convert:to_integer(V)) + Sum
              catch
                  error:badarith ->
                      Sum
              end
      end,
      0,
      Vals);
prep_totals(_, _, _) ->
    undefined.