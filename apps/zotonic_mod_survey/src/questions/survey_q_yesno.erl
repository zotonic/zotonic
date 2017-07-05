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

-module(survey_q_yesno).

-export([
    answer/3,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    to_block/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").

answer(Block, Answers, _Context) ->
    Name = proplists:get_value(name, Block),
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
    LabelYes = get_label(yes, Q, Context),
    LabelNo = get_label(no, Q, Context),
    [
     {question, proplists:get_value(prompt, Q)},
     {values, [{LabelYes, Yes}, {LabelNo, No}]},
     {type, "pie"},
     {data, [[LabelYes, YesP], [LabelNo, NoP]]}
    ].

get_label(Label, Q, Context) ->
    Trans = proplists:get_value(Label, Q, <<>>),
    maybe_default(Label, z_trans:lookup_fallback(Trans, Context), Context).

maybe_default(no, <<>>, Context) ->
    ?__("No", Context);
maybe_default(yes, <<>>, Context) ->
    ?__("Yes", Context);
maybe_default(_, Text, _Context) ->
    Text.


prep_answer_header(Q, _Context) ->
    proplists:get_value(name, Q).

prep_answer(_Q, [], _Context) ->
    <<>>;
prep_answer(_Q, [{_Name, {Value, _Text}}|_], _Context) ->
    Value.

prep_block(B, _Context) ->
    B.


to_block(Q) ->
    [
        {type, survey_yesno},
        {is_required, Q#survey_question.is_required},
        {name, z_convert:to_binary(Q#survey_question.name)},
        {prompt, z_convert:to_binary(Q#survey_question.question)}
    ].

