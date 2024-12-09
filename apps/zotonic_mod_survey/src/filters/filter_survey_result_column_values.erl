%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell

%% Copyright 2023 Marc Worrell
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

-module(filter_survey_result_column_values).

-export([
    survey_result_column_values/5
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

survey_result_column_values(_SurveyId, _AnswerId, [], _Format, _Context) ->
    [];
survey_result_column_values(SurveyId, AnswerId, Columns, Format, Context) when is_integer(AnswerId) ->
    case m_rsc:rid(SurveyId, Context) of
        undefined ->
            [];
        Id ->
            Answer = m_survey:single_result(Id, AnswerId, Context),
            survey_result_column_values(SurveyId, Answer, Columns, Format, Context)
    end;
survey_result_column_values(SurveyId, Answer, Columns, Format, Context) ->
    case m_rsc:rid(SurveyId, Context) of
        undefined ->
            [];
        Id ->
            Msg = #survey_result_column_values{
                id = Id,
                handler = m_rsc:p_no_acl(Id, <<"survey_handler">>, Context),
                format = format(Format),
                user_id = proplists:get_value(user_id, Answer),
                answer = Answer,
                columns = Columns
            },
            z_notifier:foldl(Msg, #{}, Context)
    end.

format(<<"html">>) -> html;
format(<<"text">>) -> text;
format(V) when is_atom(V) -> V.
