%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Convenience tag for rendering an inline SVG donut chart.
%% @end

-module(scomp_base_chart_donut).
-moduledoc(<<
    "Render a donut chart as inline SVG.\n\n",
    "This is a convenience wrapper around the [`chart`](scomp-base-chart) scomp.\n",
    "It accepts the same arguments and always uses `type=\"donut\"`.\n\n",
    "```django\n",
    "{% chart_donut data=[[\"Yes\", 42], [\"No\", 8]] %}\n",
    "```\n"
>>).

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) ->
    nocache.

render(Params, Vars, Context) ->
    ChartParams = [{type, donut} | proplists:delete(type, Params)],
    scomp_base_chart:render(ChartParams, Vars, Context).
