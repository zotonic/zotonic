%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-13

%% @doc Schema definition for the survey module.

%% Copyright 2011 Arjan Scherpenisse
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

-module(mod_survey_schema).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").
-export([manage_schema/2]).


%% @doc Install tables used for storing survey results
manage_schema(install, Context) ->
    z_db:create_table(survey_answer, [
                #column_def{name=id, type="serial", is_nullable=false},
                #column_def{name=survey_id, type="integer", is_nullable=false},
                #column_def{name=user_id, type="integer", is_nullable=true},
                #column_def{name=persistent, type="character varying", length=32, is_nullable=true},
                #column_def{name=question, type="character varying", length=32, is_nullable=false},
                #column_def{name=name, type="character varying", length=32, is_nullable=false},
                #column_def{name=value, type="character varying", length=80, is_nullable=true},
                #column_def{name=text, type="bytea", is_nullable=true},
                #column_def{name=created, type="timestamp", is_nullable=true}
            ], Context),

    % Add some indices and foreign keys, ignore errors
    z_db:equery("create index fki_survey_answer_survey_id on survey_answer(survey_id)", Context),
    z_db:equery("alter table survey_answer add 
                constraint fk_survey_answer_survey_id foreign key (survey_id) references rsc(id) 
                on update cascade on delete cascade", Context),

    z_db:equery("create index fki_survey_answer_user_id on survey_answer(user_id)", Context),
    z_db:equery("alter table survey_answer add 
                constraint fk_survey_answer_user_id foreign key (user_id) references rsc(id) 
                on update cascade on delete cascade", Context),

    %% For aggregating answers to survey questions (group by name)
    z_db:equery("create index survey_answer_survey_name_key on survey_answer(survey_id, name)", Context),
    z_db:equery("create index survey_answer_survey_question_key on survey_answer(survey_id, question)", Context),
    z_db:equery("create index survey_answer_survey_user_key on survey_answer(survey_id, user_id)", Context),
    z_db:equery("create index survey_answer_survey_persistent_key on survey_answer(survey_id, persistent)", Context),

    z_datamodel:manage(
      mod_survey,
      #datamodel{categories=
                 [
                  {survey, undefined, [{title, "Survey"}]},
                  {poll, survey, [{title, "Poll"}]}
                 ]}, Context),

    ok.
