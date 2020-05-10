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
                $("#statistics-run_queue").data("render", render_value);
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
                    $("#io-input").data("render", to_human);
                    $("#io-output").data("render", to_human);
                {% endjavascript %}
            </table>
        </div>
    </div>
</div>

<div class="col-md-5 col-lg-4 col-sm-6 col-xs-8">
    {% include "stat_panel/memory_usage.tpl" %}
</div>


<div class="col-md-4 col-lg-4 col-sm-4 col-xs-6">
    <div class="panel panel-default">

        <div class="panel-heading">System Usage</div>

        <div class="panel-body">

            <div>
                <div class="row">
                    <div class="col-md-6"><span class="meta">Atoms: </span><strong id="usage-atom-value">#</strong><span class="meta">%</span></div>
                    <div class="col-md-6 text-right">
                        <span id="system-atom_count">#</span>/<span id="system-atom_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-atom_count").data("render", function(v) { return v; });
                    $("#system-atom_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div id="usage-atom-progress" class="progress-bar progress-bar-success" role="progressbar"
                         style="width: 0%">
                    </div>
                </div>
            </div>

            <div>
                <div class="row">
                    <div class="col-md-6">
                        <span class="meta">Ports: </span><strong id="usage-port-value" id="usage-port-value">#</strong><span class="meta">%</span>
                    </div>
                    <div class="col-md-6 text-right">
                        <span id="system-port_count">#</span>/<span id="system-port_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-port_count").data("render", function(v) { return v; });
                    $("#system-port_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div id="usage-port-progress" class="progress-bar progress-bar-success" style="width: 0%"> </div>
                </div>
            </div>

            <div>
                <div class="row">
                    <div class="col-md-6"><span class="meta">Processes: </span><strong id="usage-process-value">#</strong><span class="meta">%</span></div>
                    <div class="col-md-6 text-right">
                        <span id="system-process_count">#</span>/<span id="system-process_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-process_count").data("render", function(v) { return v; });
                    $("#system-process_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div id="usage-process-progress" class="progress-bar progress-bar-success" role="progressbar" style="width: 0%"></div>
                </div>
            </div>

        </div>
    </div>
</div>


<div class="col-md-6 col-lg-6 col-sm-12">
  <div class="panel panel-default">
    <div class="panel-heading">Dispatch</div>
    <div class="panel-body">
        <table class="table">
            <thead>
                <tr>
                    <th>Dispatch</th>
                    <th>Req/min</th>
                    <th>In</th>
                    <th>Out</th>
                    <th>Mean Latency</th>
                    <th>99 percentile</t>
                </tr>
            </thead>
            <tbody id="the-stats">
            </tbody>
        </table>
    </div>
  </div>
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
    let collected = {};

    cotonic.broker.publish("model/ui/insert/the-stats", {initialData: "<tr><td>...</td></tr>", inner: true});

    cotonic.broker.subscribe("bridge/origin/$SYS/site/blog/broker/+what", function(msg, args) {
        switch(args.what) {
            case "subscribe":
                $("#broker-subscribe_one").html(msg.payload.one);
                break;
            case "publish":
                $("#broker-publish_one").html(msg.payload.one);
                break;
            case "session_count":
                $("#broker-session_count").html(msg.payload.count);
                break;
            case "router_info":
                $("#broker-nodes").html(msg.payload.nodes);
                $("#broker-edges").html(msg.payload.edges);
                $("#broker-wildcards").html(msg.payload.wildcards);
                $("#broker-paths").html(msg.payload.paths);
                $("#broker-destinations").html(msg.payload.destinations);
                break;
        }
    })

    cotonic.broker.subscribe("bridge/origin/$SYS/site/blog/cowmachine/+dispatch/+what", function(msg, args) {
        let c = collected[args.dispatch];
        if(!c) {
            collected[args.dispatch] = c = {};
        }
        switch(args.what) {
        case "duration":
            c["99"] = msg.payload["99"];
            c["mean"] = msg.payload["mean"];
            break;
        case "data_out":
            c["data_out_total"] = msg.payload["count"];
            c["data_out_one"] = msg.payload["one"];
            break;
        case "data_in": 
            c["data_in_total"] = msg.payload["count"];
            c["data_in_one"] = msg.payload["one"];
            break;
        case "1xx":
            c["1xx_count"] = msg.payload["count"];
            c["1xx_one"] = msg.payload["one"];
            break;
        case "2xx":
            c["2xx_count"] = msg.payload["count"];
            c["2xx_one"] = msg.payload["one"];
            break;
        case "3xx":
            c["3xx_count"] = msg.payload["count"];
            c["3xx_one"] = msg.payload["one"];
            break;
        case "4xx":
            c["4xx_count"] = msg.payload["count"];
            c["4xx_one"] = msg.payload["one"];
            break;
        case "5xx":
            c["5xx_count"] = msg.payload["count"];
            c["5xx_one"] = msg.payload["one"];
            break;
        default:
            console.log("what?", args.what);
        }

        render();
    });

    function render() {
         let rows = [];
         let keys = Object.keys(collected);
         keys.sort();
         for(let i=0; i < keys.length; i++) {
             if(!collected[keys[i]].mean)
                   continue;

             rows.push("<tr><td>" +  keys[i] + "</td>" +
                 "<td style=\"text-align:right\">" + render_requests(collected[keys[i]]) + "</td>" +

                 "<td style=\"text-align:right\">" + to_human(collected[keys[i]].data_in_one, "min") + "</td>" +
                 "<td style=\"text-align:right\">" + to_human(collected[keys[i]].data_out_one, "min") + "</td>" +

                 "<td style=\"text-align:right\">" + render_ms(collected[keys[i]].mean) + "</td>" +
                 "<td style=\"text-align:right\">" + render_ms(collected[keys[i]]["99"]) + "</td>" +
             "</tr>");
         }

         cotonic.broker.publish("model/ui/update/the-stats", rows.join("")); 
    }

    function render_requests(found) {
        const _1xx_one = (found["1xx_one"]|0);
        const _2xx_one = (found["2xx_one"]|0);
        const _3xx_one = (found["3xx_one"]|0);
        const _4xx_one = (found["4xx_one"]|0);
        const _5xx_one = (found["5xx_one"]|0);
 
        const summary = _1xx_one + _2xx_one + _3xx_one + _4xx_one + _5xx_one;

        return "<span>" + summary + "</span>";
    }

    function render_one(label, value) {
        if(!value) return "";
        return " <span>" + label + " " + value + "</span>"
    }

    function render_value(val) {
         if(!val) return "-";
         return val;
    }

    function render_ms(val) {
         if(!val) return "-";

         return (val / 1000).toFixed(3) + "<small class=\"meta\">ms</small>" 
    }

    cotonic.broker.subscribe("bridge/origin/$SYS/erlang/+entry", function(msg, args) {
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
                item.html(item.data('render')(msg.payload[datapoints[i]]));
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
