%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-16
%% @doc Simple interface to the pie3d type of Google chart.  Allows for simpler data entry.
%% Parameters:  data=[{label,value}, ...] colors=some_color
%% and then all parameters

%% Copyright 2009 Marc Worrell
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

-module(scomp_base_chart_pie3d).
-moduledoc("
Show a pie chart with 3D effect.

This scomp is just a convenient interface to the [\\{% chart\\_pie
%\\}](/id/doc_template_scomp_scomp_chart_pie#scomp-chart-pie) scomp with the `threed` argument set to true.

For example:


```django
{% chart_pie3d data=[[\"firefox\",23],
                     [\"internet explorer\", 67],
                     [\"safari\",4],
                     [\"chrome\",3],
                     [\"other\", 3]]
%}
```

Gives:


```django
<img class='google_chart' alt='google chart' src='http://chart.apis.google.com/chart?&cht=p3&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x&chxl=0:|firefox|internet%20explorer|safari|chrome|other&chxs=0,909090,10&chco=&chds=0,100&chd=t:23,67,4,3,3&chls=1,1,0' width='300' height='150'/>
```

Or, as an image:



See also

[google\\_chart](/id/doc_template_scomp_scomp_google_chart#scomp-google-chart) and [chart\\_pie](/id/doc_template_scomp_scomp_chart_pie#scomp-chart-pie).
").
-author("Marc Worrell <marc@worrell.nl").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, Vars, Context) ->
    scomp_base_chart_pie:render([{threed,true}|Params], Vars, Context).
