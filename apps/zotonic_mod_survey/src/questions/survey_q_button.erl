%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2023 Marc Worrell

%% Copyright 2012-2023 Marc Worrell
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

-module(survey_q_button).

-export([
    answer/4,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2
]).

answer(_SurveyId, Block, Answers, _Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    case proplists:get_value(Name, Answers) of
        undefined ->
            {error, missing};
        V ->
            case z_convert:to_bool(V) of
                true -> {ok, [{Name, <<"yes">>}]};
                false -> {ok, [{Name, <<"no">>}]}
            end
    end.


prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Q, Answers, Context) ->
    {Yes, No} = lists:foldr(fun({_Id, Vals}, {Y, N}) ->
                                    {proplists:get_value(<<"yes">>, Vals, 0) + Y,
                                     proplists:get_value(<<"no">>, Vals, 0) + N} end,
                            {0, 0},
                            Answers),
    Total = Yes + No,
    YesP = round(Yes * 100 / Total),
    NoP = 100 - YesP,
    #{
        <<"question">> => z_sanitize:html(maps:get(<<"prompt">>, Q, undefined), Context),
        <<"values">> => [ {Lab,Val} || {Lab,Val} <- [{"yes", Yes}, {"no", No}], Val /= 0 ],
        <<"type">> => <<"pie">>,
        <<"data">> => [ [Lab,Val] || [Lab,Val] <- [["yes", YesP], ["no", NoP]], Val /= 0 ]
    }.


prep_answer_header(Q, _Context) ->
    maps:get(<<"name">>, Q, undefined).

prep_answer(_Q, [], _Context) ->
    <<>>;
prep_answer(_Q, [{_Name, Value}|_], _Context) ->
    Value.

prep_block(B, _Context) ->
    B.

