%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-03-26
%%
%% @doc Model for accessing survey information.

%% Copyright 2010 Marc Worrell
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

-module(m_survey).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(questions, #m{value=undefined} = M, _Context) ->
    M#m{value=questions};
m_find_value(Id, #m{value=questions}, Context) ->
    case m_rsc:p(Id, survey, Context) of
        {survey, QuestionIds, Questions} ->
            question_to_value(QuestionIds, Questions, []);
        undefined -> 
            undefined
    end.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, _Context) ->
	[].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.



%% @doc Transform a list of survey questions to template friendly proplists
question_to_value([], _, Acc) ->
    lists:reverse(Acc);
question_to_value([Id|Ids], Qs, Acc) ->
    Q = proplists:get_value(Id, Qs),
    question_to_value(Ids, Qs, [question_to_value1(Id, Q)|Acc]).

    question_to_value1(Id, #survey_question{type=Type, name=Name, html=Html}) ->
        {Id, [{id, Id}, {type, Type}, {name, Name}, {html, Html}]};
    question_to_value1(Id, undefined) ->
        {Id, [{id, Id}, {type, undefined}]}.
