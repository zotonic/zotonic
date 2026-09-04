%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Deprecated compatibility tag for the former 3D pie chart.
%% @end

-module(scomp_base_chart_pie3d).
-moduledoc(<<
    "Deprecated compatibility alias for [`chart_pie`](scomp-base-chart-pie).\n\n",
    "The old external chart service is no longer used and the misleading 3D effect\n",
    "is not reproduced. New templates should use `{% chart type=\"pie\" ... %}` or\n",
    "`{% chart_pie ... %}`.\n"
>>).

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) ->
    nocache.

render(Params, Vars, Context) ->
    scomp_base_chart_pie:render(Params, Vars, Context).
