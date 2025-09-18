%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Generate a google chart for a dataset

%% Copyright 2009-2010 Marc Worrell, Konstantin Nikiforov
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

-module(scomp_base_google_chart).
-moduledoc("
Make charts with Google.

This tag interfaces to the [Google chart API](http://code.google.com/apis/chart/) to dynamically generate charts.

For example:


```erlang
{% google_chart type=\"line\" axis={axis position=\"bottom\" labels=[\"jan\",\"feb\",\"mar\"]}
   axis={axis position=\"left\" labels=[0,25,50,75,100]} data=[{data values=[20,80,33]}] %}
```

Will generate the following image tag:


```erlang
<img class='google_chart' alt='google chart'
   src='http://chart.apis.google.com/chart?&amp;cht=lc&amp;chts=909090,10&amp;chs=300x150&amp;chg=0,0,1,5&amp;chf=bg,s,ffffff|c,s,ffffff&amp;chdlp=b&amp;chbh=-3,3,7&amp;chxt=x,y&amp;chxl=0:|jan|feb|mar|1:|0|25|50|75|100&amp;chxs=0,909090,10|1,909090,10&amp;chco=&amp;chds=0,100&amp;chd=t:20,80,33&amp;chls=1,1,0'
   width='300' height='150'/>
```

[View the chart](http://chart.apis.google.com/chart?&cht=lc&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x,y&chxl=0:|jan|feb|mar|1:|0|25|50|75|100&chxs=0,909090,10|1,909090,10&chco=&chds=0,100&chd=t:20,80,33&chls=1,1,0).

Google chart accepts the following arguments:

| Argument              | Description                                                                      | Example                        |
| --------------------- | -------------------------------------------------------------------------------- | ------------------------------ |
| type                  | Kind of chart is generated. One of “line”, “sparkline”, “stacked\\\\_horizontal\\\\_bar”, “stacked\\\\_vertical\\\\_bar”, “grouped\\\\_horizontal\\\\_bar”, “grouped\\\\_vertical\\\\_bar”, “pie” or “pie3d”. Defaults to “line”. | type=”sparkline”               |
| id                    | The id of the generated image tag.                                               | id=”mychart”                   |
| class                 | CSS class of the generated image tag. The class “google\\\\_chart” is always added. | class=”chart”                  |
| style                 | CSS style attribute for the generated image tag.                                 | style=”border: 1px solid #fcc” |
| title                 | Title shown on the chart.                                                        | title=”Browser shares”         |
| color                 | Color for the title. Must be in hexadecimal, defaults to “909090”.               | color=”ffcc00”                 |
| font\\\\_size           | Font size in pixels for the title. Defaults to 10.                               | font\\\\_size=12                 |
| width                 | Width of the generated chart. Defaults to 300.                                   | width=450                      |
| height                | Height of the generated chart.                                                   | height=200                     |
| grid\\\\_x              | X axis grid step size.                                                           | grid\\\\_x=10                    |
| grid\\\\_y              | Y axis grid step size.                                                           | grid\\\\_y=10                    |
| grid\\\\_line\\\\_length  | Length of line segment for the grid lines, defaults to 1.                        | grid\\\\_line\\\\_length=1         |
| grid\\\\_blank\\\\_length | Length of gaps in the grid lines, defaults to 5.                                 | grid\\\\_blank\\\\_length=5        |
| background\\\\_color    | Background color for the complete chart. in hexadecimal, defaults to “ffffff”.   | background\\\\_color=”331133”    |
| chart\\\\_color         | Background color for the chart area. In hexadecimal, defaults to “ffffff”.       | chart\\\\_color=”113311”         |
| legend\\\\_location     | Where the legend is placed. One of “top”, “left”, “bottom” or “right”. Defaults to “bottom”. | legend\\\\_location=”right”      |
| axis                  | Description of an axis. You can given more than one axis argument. See the section [Axis styles and labels](#axis-styles-and-labels) below. |                                |
| data                  | The data to be shown. You can give more than one data argument. See [Data](#data) definition below. |                                |
| bar\\\\_space           | Space in pixels between the bar of a bar chart. Defaults to 3.                   | bar\\\\_space=5                  |
| bar\\\\_group\\\\_space   | Space in pixels between the groups of bars of a bar chart. Defaults to 7.        | bar\\\\_group\\\\_space=10         |



Axis styles and labels
----------------------

Axis styles and labels are available for line charts and bar charts.

An example for an axis argument is:


```erlang
axis={axis font_size=10 color=\"909090\" position=\"top\" labels=[\"jan\", \"feb\", \"mar\"]}
```

This defines an axis that will be displayed above the chart. It has three labels and will be displayed in a 10 pixel
font with color “909090”. You can give a multiple axis arguments and also a list of axes for each argument.

Valid arguments for an axis are:

| Argument    | Description                                                                      | Example                     |
| ----------- | -------------------------------------------------------------------------------- | --------------------------- |
| font\\\\_size | Size of the labels in pixels. Defaults to 10.                                    | font\\\\_size=12              |
| color       | Color for the label texts, in hexadecimal RGB. Defaults to “909090”.             | color=”cc0000”              |
| position    | Which axis is defined. One of “top”, “right”, “bottom” and “left”. Defaults to “top”. You can have multiple definitions for a position. | position=”bottom”           |
| labels      | A list with labels displayed. The labels will be evenly distributed over the axis. | labels=\\\\[2006,2007,2008\\\\] |



Data
----

All data definitions to be displayed. Each data definition is a record with arguments. You can supply all data sets at
once or with multiple data arguments.

Example with one data set:


```erlang
data=[{data line_width=1 min_value=1 max_value=100 values=[10,20,42,34,68,73,80]}]
```

Valid arguments for a data record are:

| Argument       | Description                                                             | Example                |
| -------------- | ----------------------------------------------------------------------- | ---------------------- |
| line\\\\_width   | The width of the line in pixels. Defaults to 1.                         | line\\\\_width=2         |
| line\\\\_length  | Length of line segment in pixels. Defaults to 1.                        | line\\\\_length=1        |
| blank\\\\_length | Length of blank segment in pixels. Defaults to 0.                       | blank\\\\_length=1       |
| min\\\\_value    | The minimum value for the data set, used for the axis. Defaults to 0.   | min\\\\_value=-100       |
| max\\\\_value    | The maximum value for the data set, used for the axis. Defaults to 100. | max\\\\_value=100        |
| color          | The color used for this data set. Hexadecimal RGB value.                | color=”ffcc00”         |
| legend         | Label for the dataset as shown in the legend.                           | legend=”monthly sales” |
| values         | The values for drawing the chart. Must be a list.                       | values=\\\\[0,10,5,8\\\\]  |

See also

tags [chart\\_pie](/id/doc_template_scomp_scomp_chart_pie#scomp-chart-pie) and [chart\\_pie3d](/id/doc_template_scomp_scomp_chart_pie3d#scomp-chart-pie3d).
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, _Vars, Context) ->
    {ok, <<>>}.

%     Class      = proplists:get_value(class, Params),
%     Style      = proplists:get_value(style, Params),
%     Id         = proplists:get_value(id, Params),
%     Title      = proplists:get_value(title, Params),
%     Color      = proplists:get_value(color, Params, "909090"),
%     FontSize   = z_convert:to_integer(proplists:get_value(font_size, Params, 10)),
%     Width      = z_convert:to_integer(proplists:get_value(width, Params, 300)),
%     Height     = z_convert:to_integer(proplists:get_value(height, Params, 150)),
%     GridX      = proplists:get_value(grid_x, Params),
%     GridY      = proplists:get_value(grid_y, Params),
%     GridLineLength  = z_convert:to_integer(proplists:get_value(grid_line_length, Params, 1)),
%     GridBlankLength = z_convert:to_integer(proplists:get_value(grid_blank_length, Params, 5)),
%     BGColor    = proplists:get_value(background_color, Params, "ffffff"),
%     ChartColor = proplists:get_value(chart_color, Params, "ffffff"),
%     LegendLoc  = proplists:get_value(legend_location, Params, "bottom"),
%     AxesArg    = proplists:get_all_values(axis, Params),
%     DataArg    = proplists:get_all_values(data, Params),
%     BarSpace   = z_convert:to_integer(proplists:get_value(bar_space, Params, 3)),
%     BarGroupSpace   = z_convert:to_integer(proplists:get_value(bar_group_space, Params, 7)),

%     AxesArg1 = lists:flatten(AxesArg),
%     DataArg1 = lists:flatten(DataArg),

%     % Path to Google API
%     Path = "https://chart.apis.google.com/chart?",

%     % Chart Type...
%     Type = [
%         "&cht=",
%         case proplists:get_value(type, Params, "line") of
%             "line" -> "lc";
%             "sparkline" -> "ls";
%             "stacked_horizontal_bar" -> "bhs";
%             "stacked_vertical_bar" -> "bvs";
%             "grouped_horizontal_bar" -> "bhg";
%             "grouped_vertical_bar" -> "bvg";
%             "pie" -> "p";
%             "pie3d" -> "p3";
%             OtherType -> z_url:url_encode(OtherType)
%         end
%     ],

%     % Title...
%     TitleText = case Title of
%                     undefined -> [];
%                     []        -> [];
%                     <<>>      -> <<>>;
%                     OtherTitle -> ["&chtt=", z_url:url_encode(OtherTitle)]
%                 end,

%     % Title Color and Font Size...
%     TitleStyle = io_lib:format("&chts=~s,~b", [z_convert:to_list(Color), FontSize]),

%     % Size...
%     Size = io_lib:format("&chs=~bx~b", [Width, Height]),

%     % Grid...
%     Grid = io_lib:format("&chg=~s,~s,~b,~b", [
%                 z_url:url_encode(z_convert:to_list(z_utils:coalesce([GridX, 0]))),
%                 z_url:url_encode(z_convert:to_list(z_utils:coalesce([GridY, 0]))),
%                 GridLineLength,
%                 GridBlankLength
%             ]),

%     % Background Colors...
%     BGColors = io_lib:format("&chf=bg,s,~s|c,s,~s", [
%                 z_url:url_encode(z_convert:to_list(BGColor)),
%                 z_url:url_encode(z_convert:to_list(ChartColor))
%             ]),

%     % Legend Location...
%     LegendLocation = "&chdlp="
%                     ++  case LegendLoc of
%                             "top"    -> "t";
%                             "left"   -> "l";
%                             "bottom" -> "b";
%                             "right"  -> "r";
%                             _  -> "r"
%                         end,

%     % Axes...
%     Axes = case AxesArg1 of
%             []          -> [];
%             AxesRecords ->
%                 ProcessedAxes = [process_axis(N - 1, lists:nth(N, AxesRecords), Context) || N <- lists:seq(1, length(AxesRecords))],

%                 AxesPositions0 = string:join([X || [X, _, _] <- ProcessedAxes], ","),
%                 AxesScaling0   =             [X || [_, X, _] <- ProcessedAxes],
%                 AxesColors0    = string:join([X || [_, _, X] <- ProcessedAxes], "|"),

%                 AxesPositions = "&chxt=" ++ z_convert:to_list( z_url:url_encode(AxesPositions0) ),
%                 AxesScaling   = AxesScaling0,  % Already urlencode in process_axis/2
%                 AxesColors    = "&chxs=" ++ z_convert:to_list( z_url:url_encode(AxesColors0) ),
%                 AxesPositions ++ AxesScaling ++ AxesColors
%         end,

%     % Data...
%     {MaxValueLength, Data} = case DataArg1 of
%                 []          -> {0, []};
%                 DataRecords ->
%                     ProcessedData = [process_data(N -1, lists:nth(N, DataRecords)) || N <- lists:seq(1, length(DataRecords))],

%                     DataColors0  = string:join([X || [X, _, _, _, _, _] <- ProcessedData], ","),
%                     DataLegends0 = string:join([X || [_, X, _, _, _, _] <- ProcessedData], "|"),
%                     DataScales0  = string:join([X || [_, _, X, _, _, _] <- ProcessedData], ","),
%                     DataStyles0  = string:join([X || [_, _, _, X, _, _] <- ProcessedData], "|"),
%                     DataValues0  = string:join([X || [_, _, _, _, X, _] <- ProcessedData], "|"),

%                     DataColors  = "&chco="  ++ z_convert:to_list( z_url:url_encode(DataColors0) ),
%                     DataLegends = "&chdl="  ++ z_convert:to_list( z_url:url_encode(DataLegends0) ),
%                     DataScales  = "&chds="  ++ z_convert:to_list( z_url:url_encode(DataScales0) ),
%                     DataStyles  = "&chls="  ++ z_convert:to_list( z_url:url_encode(DataStyles0) ),
%                     DataValues  = "&chd=t:" ++ z_convert:to_list( z_url:url_encode(DataValues0) ),

%                     DataLegends1 = case string:strip(DataLegends, both, $|) of
%                         "&chdl=" -> [];
%                         _ -> DataLegends
%                     end,

%                     {lists:max([X || [_, _, _, _, _, X] <- ProcessedData]),
%                      DataColors ++ DataLegends1 ++ DataScales ++ DataValues ++ DataStyles}
%             end,

%     % Calculate bar size...
%     BarSize = case MaxValueLength of
%                 0 ->
%                     [];
%                 _ ->
%                     DataGroupsLength = length(Data),
%                     GroupSpacerPixels = MaxValueLength * BarGroupSpace,
%                     BarSpacerPixels = MaxValueLength * (DataGroupsLength * BarSpace),
%                     AvailablePixels = case proplists:get_value(type, Params, "line") of
%                         "stacked_horizontal_bar" -> Height;
%                         "grouped_horizontal_bar" -> Height;
%                         "stacked_vertical_bar" -> Width;
%                         "grouped_vertical_bar" -> Width;
%                         _ -> 0
%                     end,
%                     IndividualBarSize = (AvailablePixels - GroupSpacerPixels - BarSpacerPixels) / (DataGroupsLength * MaxValueLength),
%                     io_lib:format("&chbh=~b,~b,~b", [trunc(IndividualBarSize), BarSpace, BarGroupSpace])
%             end,

%     ImageUri = iolist_to_binary([
%         Path, Type, TitleText, TitleStyle, Size, Grid,
%         BGColors, LegendLocation, BarSize, Axes, Data
%     ]),
%     {ok, z_tags:render_tag(
%             <<"img">>,
%             [
%                 {<<"id">>,     z_html:escape(Id)},
%                 {<<"name">>,   z_html:escape(Id)},
%                 {<<"class">>,  [<<"google_chart">>, Class]},
%                 {<<"alt">>,    <<"google chart">>},
%                 {<<"style">>,  z_html:escape(Style)},
%                 {<<"src">>,    ImageUri},
%                 {<<"width">>,  Width},
%                 {<<"height">>, Height}
%             ])}.


% process_axis(N, {axis, Axis}, Context) ->
%     FontSize = proplists:get_value(font_size, Axis, 10),
%     Color    = proplists:get_value(color, Axis, "909090"),

%     Position = case z_convert:to_list(proplists:get_value(position, Axis, "top")) of
%                     "top"    -> "t";
%                     "right"  -> "r";
%                     "bottom" -> "x";
%                     "left"   -> "y";
%                     OtherPosition -> erlang:error({unknown_axis_position, OtherPosition})
%                 end,

%     Style        = lists:flatten(io_lib:format("~b,~s,~b", [N, z_convert:to_list(Color), FontSize])),
%     Scaling      = case {proplists:get_value(labels, Axis, []), proplists:get_value(range, Axis, [])} of
%                         {LabelsP, []} when is_list(LabelsP) ->
%                             StringLabels = [make_label(X, Context) || X <- LabelsP],
%                             Labels       = integer_to_list(N) ++ ":|" ++ string:join(StringLabels, "|"),
%                             "&chxl=" ++ z_url:url_encode(Labels);

%                         {[],  Range} when is_list(Range)  ->
%                             Format = string:join(["~p" || _X <- [c | Range] ], ","),
%                             "&chxr=" ++ z_url:url_encode(
%                                             lists:flatten(io_lib:format(Format, [N | Range])));

%                         _ ->
%                             ""
%                   end,
%     [Position, Scaling, Style].


% process_data(_N, {data, Data}) ->
%     LineWidth    = proplists:get_value(line_width,  Data, 1),
%     LineLength   = proplists:get_value(line_length, Data, 1),
%     BlankLength  = proplists:get_value(blank_length, Data, 0),
%     MinValue     = proplists:get_value(min_value, Data, 0),
%     MaxValue     = proplists:get_value(max_value, Data, 100),
%     Color        = proplists:get_value(color, Data),
%     Legend       = z_convert:to_list(proplists:get_value(legend, Data)),
%     Values       = proplists:get_value(values, Data, []),

%     Scale        = lists:flatten(io_lib:format("~b,~b", [MinValue,MaxValue])),
%     StringValues = [z_convert:to_list(X) || X <- Values],
%     JoinedValues = string:join(StringValues, ","),
%     Styles       = lists:flatten(io_lib:format("~b,~b,~b", [LineWidth, LineLength, BlankLength])),
%     [flatten_color(Color), Legend, Scale, Styles, JoinedValues, length(StringValues)].

% flatten_color([A|_] = List) when is_list(A) or is_binary(A) or is_atom(A) ->
%     string:join([ z_convert:to_list(Color) || Color <- List ], ",");
% flatten_color(A) ->
%     z_convert:to_list(A).


% make_label(N, _Context) when is_integer(N) orelse is_float(N) orelse is_atom(N) ->
%     z_convert:to_list(N);
% make_label(L, Context) ->
%     z_convert:to_list(?__(L, Context)).
