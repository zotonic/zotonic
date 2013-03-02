{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block head_extra %}
    {% lib
        "js/d3.js"
        "js/charts/z_charts.js"
        "js/charts/histogram-duration.js"
        "js/charts/line-chart.js"
        "js/charts/stats_charts.js"
        "css/charts.css"
    %}
{% endblock %}

{% block content %}

    <div class="edit-header">
        <h2>{_ Site Statistics _}</h2>
        {% button text="update"
            action={script script="z_notify('update_metrics')"} %}
    </div>

    <div id="test"></div>
    <div id="graphs"></div>

    {% wire name='new_metrics'
        action={script
            script="d3.select('#graphs').call(
            z_charts.update, zEvtArgs, factory)"}
    %}

    {% wire action={script
            script="var factory = stats_chart_factory();
            z_notify('update_metrics')"} %}

    {% javascript %}

        d3.select("#test").append("div")
        .call(line_chart());

    {% endjavascript %}

{% endblock %}
