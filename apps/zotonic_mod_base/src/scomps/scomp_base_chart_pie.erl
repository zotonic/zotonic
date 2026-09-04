%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Compatibility tag for rendering an inline SVG pie chart.
%% @end

-module(scomp_base_chart_pie).
-moduledoc(<<
    "Render a pie chart as inline SVG.\n\n",
    "This is a convenience wrapper around the [`chart`](scomp-base-chart) scomp.\n",
    "It accepts the same arguments and always uses `type=\"pie\"`.\n\n",
    "```django\n",
    "{% chart_pie data=[[\"Yes\", 42], [\"No\", 8]] %}\n",
    "```\n"
>>).

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) ->
    nocache.

render(Params, Vars, Context) ->
    ChartParams = [{type, pie} | proplists:delete(type, Params)],
    scomp_base_chart:render(ChartParams, Vars, Context).
