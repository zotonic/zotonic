%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Render the first page of a survey. Mostly used when the
%% automatic start of a survey is enabled.
%% Similar to the "survey_start" postback, except that this is rendered
%% inline, so the user does not have to wait till the MQTT connection
%% is made.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(scomp_survey_survey_start).
-moduledoc("
Show the first page for a given survey (with the `id` parameter):


```django
{% survey_start id=123 %}
```

The survey page template will be rendered for the first page and shown on the place of the scomp. This scomp is useful
for surveys that are configured to start automatically.
").

-behaviour(zotonic_scomp).
-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    Update = mod_survey:survey_start(Args, Context),
    render_update(Update, Context).

render_update(#context{} = RenderContext, _Context) ->
    {ok, RenderContext};
render_update(#render{} = Render, Context) ->
    {ok, z_template:render(Render, Context)}.
