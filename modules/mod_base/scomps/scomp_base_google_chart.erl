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
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, _Vars, Context) ->
    Class      = proplists:get_value(class, Params),
    Style      = proplists:get_value(style, Params),
    Id         = proplists:get_value(id, Params),
    Title      = proplists:get_value(title, Params),
    Color      = proplists:get_value(color, Params, "909090"),
    FontSize   = z_convert:to_integer(proplists:get_value(font_size, Params, 10)),
    Width      = z_convert:to_integer(proplists:get_value(width, Params, 300)),
    Height     = z_convert:to_integer(proplists:get_value(height, Params, 150)),
    GridX      = proplists:get_value(grid_x, Params),
    GridY      = proplists:get_value(grid_y, Params),
    GridLineLength  = z_convert:to_integer(proplists:get_value(grid_line_length, Params, 1)),
    GridBlankLength = z_convert:to_integer(proplists:get_value(grid_blank_length, Params, 5)),
    BGColor    = proplists:get_value(background_color, Params, "ffffff"),
    ChartColor = proplists:get_value(chart_color, Params, "ffffff"),
    LegendLoc  = proplists:get_value(legend_location, Params, "bottom"),
    AxesArg    = proplists:get_all_values(axis, Params),
    DataArg    = proplists:get_all_values(data, Params),
    BarSpace   = z_convert:to_integer(proplists:get_value(bar_space, Params, 3)),
    BarGroupSpace   = z_convert:to_integer(proplists:get_value(bar_group_space, Params, 7)),

    AxesArg1 = lists:flatten(AxesArg),
    DataArg1 = lists:flatten(DataArg),
    
    % Path to Google API
    Path = "http://chart.apis.google.com/chart?",

    % Chart Type...
    Type = [
        "&cht=",
        case proplists:get_value(type, Params, "line") of
            "line" -> "lc";
            "sparkline" -> "ls";
            "stacked_horizontal_bar" -> "bhs";
            "stacked_vertical_bar" -> "bvs";
            "grouped_horizontal_bar" -> "bhg";
            "grouped_vertical_bar" -> "bvg";
            "pie" -> "p";
            "pie3d" -> "p3";
            OtherType -> OtherType
        end
    ],
    
    % Title...
    TitleText = case Title of
                    undefined -> [];
                    []        -> [];
                    <<>>      -> <<>>;
                    OtherTitle -> ["&chtt=", z_utils:url_encode(OtherTitle)]
                end,
    
    % Title Color and Font Size...
    TitleStyle = io_lib:format("&chts=~s,~b", [z_convert:to_list(Color), FontSize]),
    
    % Size...
    Size = io_lib:format("&chs=~bx~b", [Width, Height]),
    
    % Grid...
    Grid = io_lib:format("&chg=~s,~s,~b,~b", [
                z_convert:to_list(z_utils:coalesce([GridX, 0])),
                z_convert:to_list(z_utils:coalesce([GridY, 0])),
                GridLineLength,
                GridBlankLength
            ]),
    
    % Background Colors...
    BGColors = io_lib:format("&chf=bg,s,~s|c,s,~s", [
                z_convert:to_list(BGColor), 
                z_convert:to_list(ChartColor)
            ]),
    
    % Legend Location...
    LegendLocation = "&chdlp=" 
                    ++  case LegendLoc of
                            "top"    -> "t";
                            "left"   -> "l";
                            "bottom" -> "b";
                            "right"  -> "r";
                            _  -> "r"
                        end,
    
    % Axes...
    Axes = case AxesArg1 of 
            undefined   -> [];
            []          -> [];
            <<>>        -> <<>>;
            AxesRecords ->          
                ProcessedAxes = [process_axis(N - 1, lists:nth(N, AxesRecords), Context) || N <- lists:seq(1, length(AxesRecords))],
                
                AxesPositions = "&chxt=" ++ string:join([X || [X, _, _] <- ProcessedAxes], ","),
                AxesScaling   =                         [X || [_, X, _] <- ProcessedAxes]     ,
                AxesColors    = "&chxs=" ++ string:join([X || [_, _, X] <- ProcessedAxes], "|"),
                AxesPositions ++ AxesScaling ++ AxesColors
        end,
    
    % Data...
    Data = case DataArg1 of
                undefined   -> MaxValueLength=0, [];
                []          -> MaxValueLength=0, [];
                <<>>        -> MaxValueLength=0, [];
                DataRecords ->
                    ProcessedData = [process_data(N -1, lists:nth(N, DataRecords)) || N <- lists:seq(1, length(DataRecords))],
                    DataColors  = "&chco="  ++ string:join([X || [X, _, _, _, _, _] <- ProcessedData], ","),
                    DataLegends = "&chdl="  ++ string:join([X || [_, X, _, _, _, _] <- ProcessedData], "|"),
                    DataScales  = "&chds="  ++ string:join([X || [_, _, X, _, _, _] <- ProcessedData], ","),
                    DataStyles  = "&chls="  ++ string:join([X || [_, _, _, X, _, _] <- ProcessedData], "|"),
                    DataValues  = "&chd=t:" ++ string:join([X || [_, _, _, _, X, _] <- ProcessedData], "|"),
                    MaxValueLength = lists:max([X || [_, _, _, _, _, X] <- ProcessedData]),
                    DataLegends1 = case string:strip(DataLegends, both, $|) of
                        "&chdl=" -> [];
                        _ -> DataLegends
                    end,                
            
                    DataColors ++ DataLegends1 ++ DataScales ++ DataValues ++ DataStyles
            end,
    
    % Calculate bar size...
    BarSize = case MaxValueLength of 
                0 -> [];
                _ -> 
                    DataGroupsLength = length(Data),
                    GroupSpacerPixels = MaxValueLength * BarGroupSpace,
                    BarSpacerPixels = MaxValueLength * (DataGroupsLength * BarSpace),
                    AvailablePixels = case Type of 
                        stacked_horizontal_bar -> Height;
                        grouped_horizontal_bar -> Height;
                        stacked_vertical_bar -> Width;
                        grouped_vertical_bar -> Width;
                        _ -> 0
                    end,
                    IndividualBarSize = (AvailablePixels - GroupSpacerPixels - BarSpacerPixels) / (DataGroupsLength * MaxValueLength),
                    io_lib:format("&chbh=~b,~b,~b", [trunc(IndividualBarSize), BarSpace, BarGroupSpace])
            end,

    ImageUri = lists:flatten([Path, Type, TitleText, TitleStyle, Size, Grid, BGColors, LegendLocation, BarSize, Axes, Data]),
    {ok, z_tags:render_tag(
            <<"img">>, 
            [
                {<<"id">>,     Id},
                {<<"name">>,   Id},
                {<<"class">>,  [<<"google_chart">>, Class]},
                {<<"alt">>,    <<"google chart">>},
                {<<"style">>,  Style},
                {<<"src">>,    ImageUri},
                {<<"width">>,  Width},
                {<<"height">>, Height}
            ])}.


process_axis(N, {axis, Axis}, Context) ->
    FontSize = proplists:get_value(font_size, Axis, 10),
    Color    = proplists:get_value(color, Axis, "909090"),
    
    Position = case z_convert:to_list(proplists:get_value(position, Axis, "top")) of
                    "top"    -> "t";
                    "right"  -> "r";
                    "bottom" -> "x";
                    "left"   -> "y";
                    OtherPosition -> erlang:error({unknown_axis_position, OtherPosition})
                end,

    Style        = io_lib:format("~b,~s,~b", [N, z_convert:to_list(Color), FontSize]),
    Scaling      = case {proplists:get_value(labels, Axis, []), proplists:get_value(range, Axis, [])} of
                        {LabelsP, []} when is_list(LabelsP) -> 
                            StringLabels = [make_label(X, Context) || X <- LabelsP],
                            Labels       = integer_to_list(N) ++ ":|" ++ string:join(StringLabels, "|"),
                            "&chxl=" ++ Labels;
        
                        {[],  Range} when is_list(Range)  -> 
                            Format = string:join(["~p" || _X <- [c | Range] ], ","),
                            "&chxr=" ++ io_lib:format(Format, [N | Range]);
        
                        _ -> ""  
                  end,
    [Position, Scaling, Style].
    
    
process_data(_N, {data, Data}) ->
    LineWidth    = proplists:get_value(line_width,  Data, 1),
    LineLength   = proplists:get_value(line_length, Data, 1),
    BlankLength  = proplists:get_value(blank_length, Data, 0),
    MinValue     = proplists:get_value(min_value, Data, 0),
    MaxValue     = proplists:get_value(max_value, Data, 100),
    Color        = proplists:get_value(color, Data),
    Legend       = z_convert:to_list(proplists:get_value(legend, Data)),
    Values       = proplists:get_value(values, Data, []),
    
    Scale        = io_lib:format("~b,~b", [MinValue,MaxValue]),
    StringValues = [z_convert:to_list(X) || X <- Values],
    JoinedValues = string:join(StringValues, ","),
    Styles       = io_lib:format("~b,~b,~b", [LineWidth, LineLength, BlankLength]),
    [flatten_color(Color), Legend, Scale, Styles, JoinedValues, length(StringValues)].

flatten_color([A|_] = List) when is_list(A) or is_binary(A) or is_atom(A) ->
    string:join([ z_convert:to_list(Color) || Color <- List ], ",");
flatten_color(A) ->
    z_convert:to_list(A).


make_label(N, _Context) when is_integer(N) orelse is_float(N) orelse is_atom(N) ->
    z_convert:to_list(N);
make_label(L, Context) -> 
    z_convert:to_list(?__(L, Context)).
