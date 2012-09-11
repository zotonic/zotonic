%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Check if the questions end with a question type that is a submit button.

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

-module(filter_survey_is_submit).

-export([
    survey_is_submit/2
]).

-include("zotonic_notifications.hrl").

survey_is_submit(Qs, Context) ->
	find_first_question(lists:reverse(Qs), Context).

find_first_question([], _Context) ->
	true;
find_first_question([Q|Qs], Context) ->
	case z_notifier:first(#survey_is_submit{block=Q}, Context) of
		undefined -> find_first_question(Qs, Context);
		true -> true;
		false -> false
	end.
