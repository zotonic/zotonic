%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Return a html fragment that can be used as an example for a survey question.
%% Call in the template as {% survey_example type="likert" %} 

%% Copyright 2011 Marc Worrell
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

-module(scomp_survey_survey_example).

-behaviour(gen_scomp).
-export([vary/2, render/3]).

-include("../survey.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    Q = mod_survey:new_question(proplists:get_value(type, Args)),
    {ok, filter_replace:replace(Q#survey_question.html, ["class=\"", "class=\"nosubmit "], Context)}.
