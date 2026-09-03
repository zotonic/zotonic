{% extends "base_simple.tpl" %}

{% block title %}{_ Inline SVG chart test _}{% endblock %}

{% block _html_head %}
    {% inherit %}
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">
        .chart-test .z-chart-data th,
        .chart-test .z-chart-data td {
            padding-left: 0.75rem;
            padding-right: 0.75rem;
        }
    </style>
{% endblock %}

{% block content %}
<main class="chart-test">
    <header class="page-header">
        <h1>{_ Inline SVG chart test _}</h1>
        <p class="lead">
            {_ Server-rendered charts without JavaScript, CSS, or external services. _}
        </p>
    </header>

    <div class="row">
        <section class="col-md-4">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Pie chart _}</h2></div>
                <div class="panel-body">
                    {% chart type="pie"
                             title=_"Survey answers"
                             data=[
                                 %{ "label": _"Yes", "value": "42", "color": "#4477aa" },
                                 %{ "label": _"No", "value": "8", "color": "#ee6677" },
                                 %{ "label": _"Unknown", "value": "5", "color": "#ccbb44" }
                             ]
                    %}
                </div>
            </div>
        </section>

        <section class="col-md-4">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Donut chart with map data _}</h2></div>
                <div class="panel-body">
                    {% chart type="donut"
                             title=_"Work items"
                             data=%{"Backlog": 12, "In progress": 7, "Done": 18}
                             height=280
                    %}
                </div>
            </div>
        </section>

        <section class="col-md-4">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Single-color pie chart _}</h2></div>
                <div class="panel-body">
                    {% chart type="pie"
                             title=_"Responses using one color"
                             data=[
                                 [ _"Excellent", 35],
                                 [ _"Good", 28],
                                 [ _"Average", 21],
                                 [ _"Poor", 16]
                             ]
                             color="#4477aa"
                    %}
                </div>
            </div>
        </section>
    </div>

    <div class="row">
        <section class="col-md-6">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Horizontal bar chart _}</h2></div>
                <div class="panel-body">
                    {% chart type="horizontal_bar"
                             title=_"Net change"
                             labels=[ _"North", _"East", _"South", _"West"]
                             values=[18, -7, 0, 11]
                             label_header=_"Region"
                             value_header=_"Change"
                    %}
                </div>
            </div>
        </section>

        <section class="col-md-6">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Vertical bar chart _}</h2></div>
                <div class="panel-body">
                    {% chart type="vertical_bar"
                             title=_"Quarterly balance"
                             data=[["Q1", -3], ["Q2", 8], ["Q3", 14], ["Q4", 6]]
                             width=640
                             height=260
                             palette=["228833", "66ccee"]
                    %}
                </div>
            </div>
        </section>
    </div>

    <div class="row">
        <section class="col-md-8">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Line chart _}</h2></div>
                <div class="panel-body">
                    {% chart type="line"
                             title=_"Visitors per month"
                             labels=[
                                 _"January", _"February", _"March", _"April", _"May", _"June",
                                 _"July", _"August", _"September", _"October", _"November", _"December"
                             ]
                             values=[12, 19, 15, 28, 24, 35, 31, 42, 38, 47, 44, 53]
                             width=800
                             height=300
                             color="#aa3377"
                    %}
                </div>
            </div>
        </section>

        <section class="col-md-4">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Single value _}</h2></div>
                <div class="panel-body">
                    {% chart_donut title=_"Completion" data=[[ _"Complete", 100]] height=220 %}
                </div>
            </div>
        </section>
    </div>

    <div class="row">
        <section class="col-md-6">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Existing external data table _}</h2></div>
                <div class="panel-body">
                    {% chart type="pie"
                             title=_"Browser share"
                             data=[["Firefox", 11], ["Safari", 24], ["Chrome", 65]]
                             aria_describedby=#browser_data
                             hide_table
                    %}

                    <table id="{{ #browser_data }}" class="table table-condensed">
                        <caption>{_ Browser share data _}</caption>
                        <thead>
                            <tr>
                                <th scope="col">{_ Browser _}</th>
                                <th scope="col">{_ Share _}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr><th scope="row">Firefox</th><td>11%</td></tr>
                            <tr><th scope="row">Safari</th><td>24%</td></tr>
                            <tr><th scope="row">Chrome</th><td>65%</td></tr>
                        </tbody>
                    </table>
                </div>
            </div>
        </section>

        <section class="col-md-6">
            <div class="panel panel-default">
                <div class="panel-heading"><h2 class="panel-title">{_ Empty data _}</h2></div>
                <div class="panel-body">
                    {% chart type="line" title=_"No measurements yet" data=[] height=220 %}
                </div>
            </div>
        </section>
    </div>

    {% with [
        ["01", 12], ["02", 19], ["03", 7], ["04", 24],
        ["05", 16], ["06", 31], ["07", 11], ["08", 27],
        ["09", 14], ["10", 36], ["11", 22], ["12", 9],
        ["13", 29], ["14", 18], ["15", 34], ["16", 13],
        ["17", 26], ["18", 41], ["19", 17], ["20", 33],
        ["21", 21], ["22", 38], ["23", 15], ["24", 30],
        ["25", 44], ["26", 25], ["27", 35], ["28", 20],
        ["29", 39], ["30", 28], ["31", 46], ["32", 23]
    ] as large_chart_data %}
    <section class="panel panel-default">
        <div class="panel-heading"><h2 class="panel-title">{_ Large data set with palette shades _}</h2></div>
        <div class="panel-body">
            {% chart type="horizontal_bar"
                     title=_"Scores for 32 items"
                     data=large_chart_data
                     width=960
                     label_header=_"Item"
                     value_header=_"Score"
            %}
        </div>
    </section>

    <section class="panel panel-default">
        <div class="panel-heading"><h2 class="panel-title">{_ Large pie chart with palette shades _}</h2></div>
        <div class="panel-body">
            {% chart type="pie"
                     title=_"Scores for 32 items"
                     data=large_chart_data
                     sort="-value"
                     label_min_percent=2
                     max_pie_values=12
                     width=960
                     height=480
                     label_header=_"Item"
                     value_header=_"Score"
            %}
        </div>
    </section>
    {% endwith %}

    <section class="panel panel-default">
        <div class="panel-heading"><h2 class="panel-title">{_ Compatibility tags _}</h2></div>
        <div class="panel-body">
            <div class="row">
                <div class="col-sm-4">
                    {% chart_pie title=_"Pie alias" data=[["A", 3], ["B", 2]] height=180 %}
                </div>
                <div class="col-sm-4">
                    {% chart_donut title=_"Donut alias" data=[["A", 3], ["B", 2]] height=180 %}
                </div>
                <div class="col-sm-4">
                    {% chart_pie3d title=_"Legacy pie alias" data=[["A", 3], ["B", 2]] height=180 %}
                </div>
            </div>
        </div>
    </section>
</main>
{% endblock %}
