%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc OLD 0.8 - definitions, only used for conversion to new 0.9+ block format.

%% Copyright 2010-2011 Marc Worrell
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


%% @doc A question for in a survey
-record(survey_question, {type, name, question, text, html, parts=[], is_required=true}).

%% @doc Survey acl check, called via the normal acl notifications.
%% Actions for these checks: view_result, update_result, delete_result
-record(acl_survey, {id :: m_rsc:resource(), answer_id :: pos_integer()}).
