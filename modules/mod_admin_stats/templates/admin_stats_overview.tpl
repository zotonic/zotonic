{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block head_extra %}
    {% lib
        "js/d3.min.js"
        "js/nv.d3.min.js"
        "css/nv.d3.css"
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
    var chart = createChart();

    elem.append("h3").text(data[0].key);
    elem.append("div").selectAll("span").data(data[0].info).enter()
    .append("span")
    .text(function(d){
    return d.label + ": " + d.value + (d.unit ? " (" + d.unit + ")" : "") })
    .append("br");
    elem.append("svg")
    .datum(data)
    .call(chart);

    return chart;
    });
    }

    {% for metric in m.stats.metrics %}
        var div = graphs.append("div");
        {% if metric.type == `histogram` %}
            addGraph(histChart, div,
            [{key: "{{ metric.system }}.{{ metric.name }}",
            values: [
            {% for k, v in metric.value.histogram %}
                {label: "{{ k/1000 }}", value: {{ v }} },
            {% endfor %}
            ],
            info: [
            {label: "Min", value: {{ metric.value.min/1000 }}, unit: "ms"},
            {label: "Max", value: {{ metric.value.max/1000 }}, unit: "ms"},
            {label: "Mean (arithmetic)",
            value: {{ metric.value.arithmetic_mean/1000 }} },
            {label: "Count", value: {{ metric.value.n }} }
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
