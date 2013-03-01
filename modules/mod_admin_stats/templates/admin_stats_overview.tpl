{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block head_extra %}
    {% lib
        "js/d3.js"
        "js/charts/histogram-duration.js"
        "js/charts/charts.js"
        "css/charts.css"
    %}
{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ Site Statistics _}<h2>
</div>

{% button text="update" action={script script="z_notify('get_stats')"} %}

<div id="graphs"></div>

{% javascript %}

    var graphs = d3.select("#graphs");

    var data = {% include "_metrics_jsdata.tpl" %};

    log("initial stats data: ", data.length, ": ", data);
    graphs.call(update_charts, data);

    z_event_register("stats_data", function(d) {
    log("got stats data: ", d.length, ": ", d);
    graphs.call(update_charts, d);
    });

    {# for metric in m.stats.metrics %}
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
            {label: "Mean (harmonic)",
            value: {{ metric.value.harmonic_mean/1000 }} },
            {label: "Count", value: {{ metric.value.n }} },
            /*{label: "Data", value: "{{ metric.value.histogram }}" }*/
            ]}]);
        {% else %}
            {# addGraph(countChart, div,
            [{key: "{{ metric.system }}.{{ metric.name }}",
            values: [
            {% for l in [`one`, `five`, `fifteen`, `day`] %}
                {label: "{{ l }}", value: {{ metric.value[l] }} },
            {% endfor %}
            ],
            info: [
            {label: "Count", value: {{ metric.value.count }} },
            {label: "Mean", value: {{ metric.value.mean }} }
            ]}]) # }
        {% endif %}
    {% endfor #}

{% endjavascript %}

{% endblock %}
