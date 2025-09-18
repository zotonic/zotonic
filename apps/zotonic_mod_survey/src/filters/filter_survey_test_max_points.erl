%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Calculate the maximu points for a test survey.

%% Copyright 2017 Marc Worrell
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

-module(filter_survey_test_max_points).
-moduledoc("
Counts the total of all points that can be received for all *test* questions. Non *test* questions are not counted.

Usage:


```django
{{ survey_id|survey_test_max_points }}
```

Return the number of points if all questions are corectly answered.

See [mod\\_survey](/id/doc_module_mod_survey)
").

-export([
    survey_test_max_points/2
]).

survey_test_max_points(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> 0;
        RId -> survey_test_results:max_points(RId, Context)
    end.

