%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Render a chart as bounded inline SVG and an optional data table.
%% @end

-module(scomp_base_chart).
-moduledoc(<<
    "Render a chart without JavaScript, CSS, external services, or external assets.\n\n",
    "The `data` argument can be a list of `{Label, Value}` tuples, `[Label, Value]`\n",
    "lists, or maps with `label` and `value` keys and an optional `color`. Numeric\n",
    "values can be numbers or strings. It can also be a map where each key is a\n",
    "label and each value is its chart value:\n\n",
    "For example:\n\n",
    "```django\n",
    "{% chart type=\"pie\"\n",
    "         title=_\"Responses\"\n",
    "         data=[[\"Yes\", 42], [\"No\", 8]] %}\n\n",
    "{% chart type=\"pie\" data=[\n",
    "    %{ \"label\": _\"Yes\", \"value\": \"42\", \"color\": \"#4477aa\" },\n",
    "    %{ \"label\": _\"No\", \"value\": \"8\" }\n",
    "] %}\n\n",
    "{% chart type=\"pie\" data=%{\"Yes\": 42, \"No\": 8} %}\n",
    "```\n\n",
    "If `data` is not supplied, the `labels` and `values` lists are zipped. Values\n",
    "must be finite numbers. Generated data tables include the same color swatches\n",
    "as categorical charts, so every value can be matched to its chart mark.\n\n",
    "Supported types are `pie`, `donut`, `horizontal_bar`, `vertical_bar`, and\n",
    "`line`. The optional arguments are:\n\n",
    "| Argument | Description |\n",
    "| --- | --- |\n",
    "| `title` | Accessible chart title and table caption. |\n",
    "| `data` | Label/value rows or a map of labels to values. Row maps can set `color`. |\n",
    "| `labels` | Labels to zip with `values` when `data` is not supplied. |\n",
    "| `values` | Values to zip with `labels` when `data` is not supplied. |\n",
    "| `width` | View box width, clamped between 64 and 4096. Defaults to 400. |\n",
    "| `height` | View box height, clamped between 64 and 4096. Defaults to 240. |\n",
    "| `colors` | List of hexadecimal RGB colors. Later points use shades when the list is exhausted. Invalid colors are ignored. |\n",
    "| `sort` | Sort with `-value`, `+value`, `-label`, or `+label`. A missing sign means ascending; the bare flag means `-value`. |\n",
    "| `class` | Additional classes on the generated `figure`. |\n",
    "| `label_header` | Header for the label column in the data table. |\n",
    "| `value_header` | Header for the value column in the data table. |\n",
    "| `hide_table` | Do not generate a table. Use only when the data is already shown in a table. |\n",
    "| `legend` | `auto` by default. With a hidden table, pie and donut charts get a color key. Set to true or false to override. |\n",
    "| `aria_describedby` | Id of an existing table or description for the SVG. |\n\n",
    "For security and predictable rendering, at most 256 data points are used,\n",
    "labels are truncated, arbitrary SVG/CSS is not accepted, and the component is\n",
    "never cached independently. The component only renders supplied data and does\n",
    "not perform data access or authorization.\n"
>>).

-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

vary(_Params, _Context) ->
    nocache.

render(Params, _Vars, Context) ->
    case z_chart_svg:render(Params, Context) of
        {ok, Html} -> {ok, Html};
        {error, badarg} -> {ok, <<>>}
    end.
