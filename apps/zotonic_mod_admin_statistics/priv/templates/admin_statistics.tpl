{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

<style>
.meta {
    font-weight: bold;
    color: #888;
}
</style>

<div class="row">

<div class="col-md-3 col-lg-3 col-sm-3 col-xs-6">
  <div class="panel panel-default">
    <div class="panel-heading">Run Queue</div>

    <div class="panel-body">
        <table class="table table-condensed">
            <thead></thead>
            <tbody>
                {% for title, id in [ ["Run Queue", "statistics-run_queue"] ] %}
                    {% include "_stat_row.tpl" %}
                {% endfor %}
            </tbody>
            {% javascript %}
                // $("#statistics-run_queue").data("render", render_value);
            {% endjavascript %}
        </table>
    </div>
  </div>
</div>

<div class="col-md-3 col-lg-3 col-sm-3 col-xs-6">
    <div class="panel panel-default">
        <div class="panel-heading">Broker</div>
        <div class="panel-body">
            <table class="table table-condensed">
                <thead></thead>
                <tbody>
                    {% for title, id in [ ["Sessions", "broker-session_count"],
                                          ["Subscribes/min", "broker-subscribe_one"],
                                          ["Publishes/min", "broker-publish_one"],
                                          ["Subscribers", "broker-destinations"],
                                          ["Nodes", "broker-nodes"],
                                          ["Edges", "broker-edges"],
                                          ["Wildcards", "broker-wildcards"],
                                          ["Paths", "broker-paths"] ] %}
                        {% include "_stat_row.tpl" %}
                    {% endfor %}
                </tbody>
            </table>
        </div>
    </div>

    {% include "stat_panel/database.tpl" %}
</div>

<div class="col-md-3 col-lg-3 col-sm-3 col-xs-6">
    <div class="panel panel-default">
        <div class="panel-heading">IO</div>
        <div class="panel-body">
            <table class="table table-condensed">
                <thead></thead>
                <tbody>
                    {% for title, id in [ ["Input", "io-input"],
                                          ["Output", "io-output"] ] %}
                        {% include "_stat_row.tpl" %}
                    {% endfor %}
                </tbody>
                {% javascript %}
                    // $("#io-input").data("render", to_human);
                    // $("#io-output").data("render", to_human);
                {% endjavascript %}
            </table>
        </div>
    </div>
</div>

<div class="col-md-5 col-lg-4 col-sm-6 col-xs-8">
    {% include "stat_panel/memory_usage.tpl" %}
</div>

<div class="col-md-4 col-lg-4 col-sm-4 col-xs-6">
    {% include "stat_panel/system_usage.tpl" %}
</div>

<div class="col-md-6 col-lg-6 col-sm-12">
    {% include "stat_panel/dispatch.tpl" %}
</div>


</div>

{% javascript %}
function to_human(value, per) {
    if(value === undefined)
        return "-";

    if(value < 512) // .5 kb
        return value + " " + unit("B", per);

    if(value < 524288) // .5 Mb
        return (value/1024).toFixed(1) + unit("Kb", per);

    if(value < 536870912) // .5 Gb
        return (value/1024/1024).toFixed(1) + unit("Mb", per);

    if(value < 549755813888) // *.5 Tb
        return (value/1024/1024/1024).toFixed(1) + unit("Gb", per);

    return (value/1024/1024/1024/1024).toFixed(1) + unit("Tb", per);
}

function unit(u, per) {
    if(per) {
        return " <small class=\"meta\">" + u + "/" + per + "</small>";
    }
    return " <small class=\"meta\">" + u + "</small>";
}
{% endjavascript %}

{% javascript %}
function render_value(val) {
    if(!val) return "-";
    return val;
}

function render_ms(val) {
    if(!val) return "-";

    return (val / 1000).toFixed(3) + "<small class=\"meta\">ms</small>"
}

cotonic.broker.subscribe("bridge/origin/$SYS/erlang/+entry",
     function(msg, args) {
        if(args.entry === "usage") {
            update_usage(msg.payload);
            return;
        }

        const datapoints = Object.keys(msg.payload);

        for(let i=0; i < datapoints.length; i++) {
            const itemId = "#" + args.entry + "-" + datapoints[i];

            let item = $(itemId);
            if(item.length > 0) {
                // console.log(itemId, msg.payload[datapoints[i]]);
                // item.html(item.data('render')(msg.payload[datapoints[i]]));
            } else {
                console.log("No place for", itemId, msg.payload[datapoints[i]]);
            }
        }
});

function update_usage(dp) {
    const keys = Object.keys(dp);
    for(let i=0; i < keys.length; i++) {
        const key = keys[i];

        $("#usage-" + key + "-value").html(dp[key]);
        $("#usage-" + key + "-progress").css({width: dp[key].toString() + "%"});
    }
}
{% endjavascript %}

{% endblock %}
