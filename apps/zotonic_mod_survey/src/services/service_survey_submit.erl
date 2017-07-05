%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Programmatically submit a survey over POST.

%% Copyright 2015 Arjan Scherpenisse
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

-module(service_survey_submit).

-svc_title("Submit a survey.").
-svc_needauth(false).

-export([process_post/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

process_post(Context) ->
    {B, _} = cowmachine_req:req_body(Context),
    case z_context:get_q(<<"id">>, Context) of
        Id when Id =:= undefined; Id =:= <<>> ->
            {error, missing_arg, "id"};

        IdStr ->
            case m_rsc:rid(IdStr, Context) of
                SurveyId when is_integer(SurveyId) ->
                    {struct, Answers} = mochijson2:decode(B),

                    case m_rsc:p(SurveyId, blocks, Context) of
                        Questions when is_list(Questions) ->

                            {_, Missing} = mod_survey:collect_answers(Questions, Answers, Context),
                            case Missing =:= [] orelse z_convert:to_bool(z_context:get_q(<<"allow_missing">>, Context)) of
                                true ->
                                    Result = mod_survey:do_submit(SurveyId, Questions, Answers, Context),
                                    handle_survey_result(Result, Context);
                                false ->
                                    Ms = string:join([z_convert:to_list(M) || M <- Missing], ", "),
                                    {error, syntax, "Missing fields: " ++ Ms}
                            end;
                        _ ->
                            {error, not_exists, "survey"}
                    end;
                undefined ->
                    {error, not_exists, IdStr}
            end
    end.

handle_survey_result(ok, _Context) ->
    {struct, [{result, "submitted"}]};
handle_survey_result({ok, _}, _Context) ->
    {struct, [{result, "submitted"}]};
handle_survey_result({error, Reason}, _) ->
    R = lists:flatten(io_lib:format("~p", [Reason])),
    {struct, [{result, "error"}, {reason, R}]}.



