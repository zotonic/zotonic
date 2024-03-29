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

-module(survey_q_narrative).

-export([
    answer/4,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    to_block/1
]).

-include_lib("zotonic_mod_survey/include/survey.hrl").

to_block(Q) ->
    #{
        <<"type">> => <<"survey_narrative">>,
        <<"is_required">> => Q#survey_question.is_required,
        <<"name">> => z_convert:to_binary(Q#survey_question.name),
        <<"narrative">> => z_convert:to_binary(Q#survey_question.text)
    }.


answer(_SurveyId, Block, Answers, Context) ->
    Narrative = z_trans:lookup_fallback(
                    maps:get(<<"narrative">>, Block, <<>>),
                    Context),
    {_Parts, Inputs} = filter_survey_prepare_narrative:parse(Narrative),
    answer_inputs(Inputs, Answers, []).

answer_inputs([], _Answers, Acc) ->
    {ok, Acc};
answer_inputs([{_IsSelect,Name}|Rest], Answers, Acc) ->
    NameB = z_convert:to_binary(Name),
    case proplists:get_value(NameB, Answers) of
        undefined -> {error, missing};
        Value ->
            case z_string:trim(Value) of
                <<>> -> {error, missing};
                V ->
                    answer_inputs(Rest, Answers, [{NameB,V}|Acc])
            end
    end.



prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, Answers, Context) ->
    Narrative = z_trans:lookup_fallback(
                    maps:get(<<"narrative">>, Block, <<>>),
                    Context),
    {Parts, _Inputs} = filter_survey_prepare_narrative:parse(Narrative),
    #{
        <<"question">> => result_title(Parts, []),
        <<"charts">> => [ prep_chart1(Parts, Ans) || Ans <- Answers ]
    }.

prep_chart1(Parts, {Name, Vals}) ->
        case find_select(Parts, Name) of
            {select, Nm, Labels} ->
                {LabelIndices, LabelLabels} = lists:unzip(Labels),
                Values = [ proplists:get_value(z_convert:to_binary(C), Vals, 0) || C <- LabelIndices ],
                Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
                Perc = [ round(V*100/Sum) || V <- Values ],
                #{
                    <<"name">> => Nm,
                    <<"values">> => lists:zip(LabelLabels, Values),
                    <<"type">> => <<"pie">>,
                    <<"data">> => [{L,P} || {L,P} <- lists:zip(LabelLabels, Perc), P /= 0]
                };
            _ ->
                undefined
        end.


prep_answer_header(Block, _Context) ->
    Parts = maps:get(<<"parts">>, Block, []),
    [ z_convert:to_binary(Name) || {_, Name, _} <- Parts ].

prep_answer(Block, Answers, _Context) ->
    Parts = maps:get(<<"parts">>, Block, []),
    [ z_convert:to_binary(value(Type, proplists:get_value(z_convert:to_binary(Name), Answers), TypeArgs)) || {Type, Name, TypeArgs} <- Parts ].

is_answerable({html, _, _}) -> false;
is_answerable(_) -> true.

value(_, undefined, _) -> <<>>;
value(select, V, Options) -> proplists:get_value(V, Options, V);
value(_, X, _) -> X.

prep_block(Block, Context) ->
    Narrative = z_trans:lookup_fallback(
                    maps:get(<<"narrative">>, Block, <<>>),
                    Context),
    {Parts0, _Inputs} = filter_survey_prepare_narrative:parse(Narrative),
    Parts = [ Part || Part <- Parts0, is_answerable(Part) ],
    Block#{ <<"parts">> => Parts }.



find_select([], _Name) ->
    undefined;
find_select([{select, Sel, _} = S|Rest], Name) ->
    case z_convert:to_binary(Sel) == Name of
        true -> S;
        false -> find_select(Rest, Name)
    end;
find_select([_|Rest], Name) ->
    find_select(Rest, Name).


result_title([], Acc) ->
     lists:reverse(Acc);
result_title([{input, _, _}|Parts], Acc) ->
     result_title(Parts, [<<"…"/utf8>>|Acc]);
result_title([{html, _, Html}|Parts], Acc) ->
     result_title(Parts, [Html|Acc]);
result_title([{select, Name, _}|Parts], Acc) ->
     result_title(Parts, [[$[,Name,$]]|Acc]).

