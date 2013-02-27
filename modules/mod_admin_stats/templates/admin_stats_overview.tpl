{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block head_extra %}
    {% lib
        "js/d3.js"
        "js/nv.d3.js"
        "css/nv.d3.css"
        "js/charts/histogram-duration.js"
        "css/charts.css"
    %}
{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ Site Statistics _}<h2>
</div>

{#
{% for metric in m.stats.metrics %}

    {{ metric.system }}.{{ metric.name }}<br />
    {{ metric.value|pprint }}<br />
    <br />

{% endfor %}
#}

<div id="graphs"></div>

{% javascript %}

    var graphs = d3.select("#graphs");

    /* var div = graphs.append("div");
    div.append("h3").text("test chart");
    var chart = histogram_duration_chart()
    //.data([{x: 2, y: 5}, {x: 4, y: 50}, {x: 6, y: 25}]);
    //chart.axis().x.tickValues([0, 2, 4, 6]);
    //chart.format().x = d3.format(",.1f");
    div.call(chart);*/

    function duraChart(data) {
    var chart = histogram_duration_chart();
    chart.format().x = d3.format(",.1f");
    var ticks = [], i, p, x, r;
    r = (data[0].values[data[0].values.length - 1].x - data[0].values[0].x);
    r /= 10;
    p = -r; log(r);
    for( i = 0; i < data[0].values.length; i++)
    {
    x = data[0].values[i].x;
    if (x - p < r) continue;
    ticks.push(x);
    p = x;
    }
    chart.axis().x.tickValues(ticks);
    return chart;
    }

    function histChart() {
    return nv.models.discreteBarChart()
    .x(function(d){ return d.label })
    .y(function(d){ return d.value })
    .staggerLabels(true);
    }

    function countChart() {
    return nv.models.discreteBarChart()
    .x(function(d){ return d.label })
    .y(function(d){ return d.value });
    //.staggerLabels(true);
    }

    function addGraph(createChart, elem, data) {
    nv.addGraph(function() {
    var chart = createChart(data);

    elem.append("h3").text(data[0].key);
    elem.append("div").selectAll("span").data(data[0].info).enter()
    .append("span")
    .text(function(d){
    return d.label + ": " + d.value + (d.unit ? " (" + d.unit + ")" : "") })
    .append("br");
    if (!chart.data) {
    elem.append("svg")
    .datum(data)
    .call(chart);
    } else
    chart.data(data[0].values)(elem);

    return chart;
    });
    }

    {% for metric in m.stats.metrics %}
        var div = graphs.append("div");
        {% if metric.type == `histogram` %}
            addGraph(duraChart, div,
            [{key: "{{ metric.system }}.{{ metric.name }}",
            values: [
            {% for k, v in metric.value.histogram %}
                {x: {{ k/1000 }}, y: {{ v }} },
            {% endfor %}
            ],
            info: [
            {label: "Min", value: {{ metric.value.min/1000 }}, unit: "ms"},
            {label: "Max", value: {{ metric.value.max/1000 }}, unit: "ms"},
            {label: "Mean (arithmetic)",
            value: {{ metric.value.arithmetic_mean/1000 }} },
            {label: "Count", value: {{ metric.value.n }} },
            /*{label: "Data", value: "{{ metric.value.histogram }}" }*/
            ]}]);
        {% else %}
            addGraph(countChart, div,
            [{key: "{{ metric.system }}.{{ metric.name }}",
            values: [
            {% for l in [`one`, `five`, `fifteen`, `day`] %}
                {label: "{{ l }}", value: {{ metric.value[l] }} },
            {% endfor %}
            ],
            info: [
            {label: "Count", value: {{ metric.value.count }} },
            {label: "Mean", value: {{ metric.value.mean }} }
            ]}])
        {% endif %}
    {% endfor %}

{% endjavascript %}

{% endblock %}
