%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell

%% Copyright 2012 Marc Worrell
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
    answer/3,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2
]).

-include("zotonic.hrl").
-include("../survey.hrl").


answer(Block, Answers, _Context) ->
    Name = proplists:get_value(name, Block),
    case proplists:get_value(Name, Answers) of
        undefined -> 
            {error, missing};
        CatId when is_binary(CatId) ->
            {ok, [{Name, CatId}]};
        List when is_list(List) -> 
            Flattened = string:join([ z_convert:to_list(V) || V <- List, V /= <<>> ], "#"),
            {ok, [{Name, {text, list_to_binary(Flattened)}}]}
    end.


prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, [{Name, {text, Vals0}}], Context) ->
    prep_chart(Block, [{Name, Vals0}], Context);
prep_chart(Block, [{_, Vals}], Context) ->
    CatName = proplists:get_value(category, Block),
    {Titles, Labels0} = lists:unzip(all_in_cat(CatName, Context)),
    Labels = [ z_convert:to_binary(Id) || Id <- Labels0 ],
    Values = [ proplists:get_value(C, Vals, 0) || C <- Labels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    [
        {question, z_html:escape(proplists:get_value(prompt, Block), Context)},
        {values, lists:zip(Titles, Values)},
        {type, "pie"},
        {data, [{L,P} || {L,P} <- lists:zip(Titles, Perc), P /= 0]}
    ].

all_in_cat(CatName, Context) ->
    #search_result{result=List} = z_search:search({all_bytitle, [{cat, CatName}]}, Context),
    List.

prep_answer_header(Q, Context) ->
    Name = proplists:get_value(name, Q),
    CatName = proplists:get_value(category, Q),
    Labels = [ z_convert:to_binary(Id) || {_Title,Id} <- all_in_cat(CatName, Context)],
    case z_convert:to_bool(proplists:get_value(is_multiple, Q)) of
        true -> [ <<Name/binary, $:, K/binary>> || {K,_} <- Labels ];
        false -> Name
    end.

prep_answer(Q, [], _Context) ->
    prep(Q, []);
prep_answer(Q, [{_Name, {undefined, Text}}], _Context) ->
    prep(Q, binary:split(Text, <<$#>>, [global]));
prep_answer(Q, [{_Name, {Value, _Text}}], _Context) ->
    prep(Q, [Value]).

    prep(Q, Vs) ->
        case z_convert:to_bool(proplists:get_value(is_multiple, Q)) of
            false ->
                hd(Vs);
            true ->
                [
                    case lists:member(K, Vs) of
                        true -> K;
                        false -> <<>>
                    end
                    || {K, _} <- proplists:get_value(answers, Q)
                ]
        end.
    
    
prep_block(Block, _Context) ->
    Block.

