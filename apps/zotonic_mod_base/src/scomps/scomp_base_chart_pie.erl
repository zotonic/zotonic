%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-16
%% @doc Simple interface to the pie type of Google chart.  Allows for simpler data entry.
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

-module(scomp_base_chart_pie).
-moduledoc("
Show a pie chart.

This is an utility tag providing a simplified interface to the pie chart feature of the [\\{% google\\_chart
%\\}](/id/doc_template_scomp_scomp_google_chart#scomp-google-chart) tag. It has an easier way to define the data.

Example of simple pie chart:


```django
{% chart_pie data=[[\"firefox\",23],
                   [\"internet explorer\", 67],
                   [\"safari\",4],
                   [\"chrome\",3],
                   [\"other\", 3]]
%}
```

This generates the following image tag:


```django
<img class='google_chart' alt='google chart'
   src='http://chart.apis.google.com/chart?&cht=p&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x&chxl=0:|firefox|internet%20explorer|safari|chrome|other&chxs=0,909090,10&chco=&chds=0,100&chd=t:23,67,4,3,3&chls=1,1,0' width='300' height='150' />
```

Or, as an image:



The tag chart\\_pie accepts the following arguments:

| Argument | Description                                                                      | Example                                 |
| -------- | -------------------------------------------------------------------------------- | --------------------------------------- |
| data     | The data for the pie chart. A list of pairs of \\\\{label, value\\\\} or \\\\[label, value\\\\]. | \\\\[\\\\{“nl”,300\\\\},\\\\{uk,”200”\\\\}\\\\]     |
| colors   | The colors for the pies. A list of colors, when there are more data points than colors then the colors are interpolated. Colors are specified in hexadecimal. Defaults to Google default colors. | colors=\\\\[“ffcc00”,”ccff00”,”00ffcc”\\\\] |
| threed   | Set to true to have a 3D effect on the pie chart. Defaults to false.             | threed=true                             |
| width    | The width of the generated pie chart, in pixels. Defaults to 300.                | width=450                               |
| height   | The height of the generated pie chart, in pixels. Defaults to 150.               | height=200                              |

Other arguments can be found at the [google\\_chart](/id/doc_template_scomp_scomp_google_chart#scomp-google-chart) tag.

See also

[google\\_chart](/id/doc_template_scomp_scomp_google_chart#scomp-google-chart) and [chart\\_pie3d](/id/doc_template_scomp_scomp_chart_pie3d#scomp-chart-pie3d).
").
-author("Marc Worrell <marc@worrell.nl").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) -> default.

render(_Params, _Vars, _Context) ->
    {ok, <<>>}.

    % Data   = proplists:get_value(data, Params, []),
    % Colors = proplists:get_value(colors, Params),
    % ThreeD = proplists:get_value(threed, Params, false),

    % Params1 = proplists:delete(labels,
    %             proplists:delete(data,
    %                 proplists:delete(colors,
    %                     proplists:delete(threed, Params)))),

    % {Labels,Values} = case Data of
    %     [] -> {[],[]};
    %     [{_,_}|_] -> lists:unzip(Data);
    %     [[_,_]|_] -> lists:foldr(fun([A,B],{Acc,Bcc}) -> {[A|Acc],[B|Bcc]} end, {[],[]}, Data)
    % end,
    % Axes  = [ {axis, [{position,"bottom"},{labels,Labels}]} ],
    % Type  = case z_template_compiler_runtime:to_bool(ThreeD, Context) of
    %     true -> "pie3d";
    %     false -> "pie"
    % end,

    % Data2 = case is_list(Colors) of
    %     true ->  [{data, [{values, Values}, {color, Colors}]}];
    %     false -> [{data, [{values, Values}]}]
    % end,

    % Params2 = [{axis,Axes}, {type,Type}, {data,Data2} | Params1],
    % scomp_base_google_chart:render(Params2, Vars, Context).
