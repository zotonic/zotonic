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

-module(survey_q_category).

-export([
    answer/4,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

answer(_SurveyId, Block, Answers, _Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    case proplists:get_value(Name, Answers) of
        undefined ->
            {error, missing};
        RscId when is_binary(RscId) ->
            {ok, [{Name, [RscId]}]};
        List when is_list(List) ->
            Flattened = [ V || V <- List, V /= <<>> ],
            {ok, [{Name, Flattened}]}
    end.


prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, [{Name, {text, Vals0}}], Context) ->
    prep_chart(Block, [{Name, Vals0}], Context);
prep_chart(Block, [{_, Vals}], Context) ->
    CatName = maps:get(<<"category">>, Block, undefined),
    {Titles, Labels0} = lists:unzip(all_in_cat(CatName, Context)),
    Labels = [ z_convert:to_binary(Id) || Id <- Labels0 ],
    Values = [ proplists:get_value(C, Vals, 0) || C <- Labels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    #{
        <<"question">> => maps:get(<<"prompt">>, Block, undefined),
        <<"values">> => lists:zip(Titles, Values),
        <<"type">> => <<"pie">>,
        <<"data">> => [{L,P} || {L,P} <- lists:zip(Titles, Perc), P /= 0]
    }.

all_in_cat(CatName, Context) ->
    #search_result{result=List} = z_search:search(<<"all_bytitle">>, #{ <<"cat">> => CatName }, 1, 1000, Context),
    List.

prep_answer_header(Q, Context) ->
    Name = maps:get(<<"name">>, Q, undefined),
    CatName = maps:get(<<"category">>, Q, undefined),
    Labels = [ z_convert:to_binary(Id) || {_Title,Id} <- all_in_cat(CatName, Context)],
    case z_convert:to_bool(maps:get(<<"is_multiple">>, Q, false)) of
        true -> [ <<Name/binary, $:, Label/binary>> || Label <- Labels ];
        false -> Name
    end.

prep_answer(Q, [], _Context) ->
    prep(Q, []);
prep_answer(Q, [{_Name, Value}|_], _Context) when is_list(Value) ->
    prep(Q, Value);
prep_answer(Q, [{_Name, Value}|_], _Context) ->
    prep(Q, [Value]).

prep(Q, Vs) ->
    case z_convert:to_bool(maps:get(<<"is_multiple">>, Q, false)) of
        false ->
            hd(Vs);
        true ->
            [
                case lists:member(K, Vs) of
                    true -> K;
                    false -> <<>>
                end
                || {K, _} <- maps:get(<<"answers">>, Q, [])
            ]
    end.


prep_block(Block, _Context) ->
    Block.

