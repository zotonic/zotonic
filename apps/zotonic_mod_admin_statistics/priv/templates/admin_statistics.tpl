{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

<div class="row">

<div class="col-md-2 col-lg-2 col-sm-2 col-xs-4">
  <div class="panel panel-default">
    <div class="panel-heading">Run Queue</div>
    <div class="panel-body">
        {% for id, name in [ ["statistics-run_queue", "Run Queue"] ] %}
            {% include "_stat_info.tpl" id=id name=name %}
        {% endfor %}
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-4 col-xs-6">
    <div class="panel panel-default">

        <div class="panel-heading">System Usage</div>

        <div class="panel-body">

            <div id="usage-atom">
                <div class="row">
                    <div class="col-md-6">Atoms: <strong class="value">#</strong>%</div>
                    <div class="col-md-6 text-right">
                        <span id="system-atom_count">#</span>/<span id="system-atom_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-atom_count").data("render", function(v) { return v; });
                    $("#system-atom_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div class="progress-bar progress-bar-success" role="progressbar"
                         aria-valuenow="1"
                         aria-valuemin="0"
                         aria-valuemax="100"
                         style="width: 0%">
                    </div>
                </div>
            </div>

            <div id="usage-port">
                <div class="row">
                    <div class="col-md-6">Ports: <strong class="value">#</strong>%</div>
                    <div class="col-md-6 text-right">
                        <span id="system-port_count">#</span>/<span id="system-port_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-port_count").data("render", function(v) { return v; });
                    $("#system-port_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div class="progress-bar progress-bar-success"
                         role="progressbar"
                         aria-valuenow="7"
                         aria-valuemin="0"
                         aria-valuemax="100"
                         style="width: 0%">
                    </div>
                </div>
            </div>

            <div id="usage-process">
                <div class="row">
                    <div class="col-md-6">Processes: <strong class="value">#</strong>%</div>
                    <div class="col-md-6 text-right">
                        <span id="system-process_count">#</span>/<span id="system-process_limit">#</span> 
                    </div>
                </div>
                {% javascript %}
                    $("#system-process_count").data("render", function(v) { return v; });
                    $("#system-process_limit").data("render", function(v) { return v; });
                {% endjavascript %}
                <div class="progress">
                    <div class="progress-bar progress-bar-success" role="progressbar"
                         aria-valuenow="0"
                         aria-valuemin="0"
                         aria-valuemax="100"
                         style="width: 0%">
                    </div>
                </div>
            </div>

        </div>
    </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-5">
  <div class="panel panel-default">
    <div class="panel-heading">I/O</div>
    <div class="panel-body">
        {% for id, name in [
                ["io-input", "Input"],
                ["io-output", "Output"]] %}
            {% include "_stat_info.tpl" id=id name=name render="to_human" %}
        {% endfor %}

        {% for id, name in [
                ["network-tcp_port_count", "Open TCP Connections"]] %}
            {% include "_stat_info.tpl" id=id name=name %}
        {% endfor %}
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-5">
  <div class="panel panel-default">
    <div class="panel-heading">Memory</div>
    <div class="panel-body">
      {% for id, name in [
                ["memory-total", "Total"],
                ["memory-binary", "Binary"],
                ["memory-ets", "Ets"],
                ["memory-atom", "Atom"],
                ["memory-processes", "Processes"],
                ["memory-system", "System"]]  %}
            {% include "_stat_info.tpl" id=id name=name render="to_human" %}
        {% endfor %}
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
function to_human(value) {
    if(value < 512) // .5 kb
        return value + "<small>B</small>";

    if(value < 524288) // .5 Mb
        return (value/1024).toFixed(1) + "<small>Kb</small>";

    if(value < 536870912) // .5 Gb
        return (value/1024/1024).toFixed(1) + "<small>Mb</small>";

    if(value < 549755813888) // *.5 Tb
        return (value/1024/1024/1024).toFixed(1) + "<small>Gb</small>";

    return (value/1024/1024/1024/1024).toFixed(1) + "<small>Tb</small>";
}
{% endjavascript %}

{% javascript %}
    let collected = {};

    cotonic.broker.publish("model/ui/insert/the-stats", {initialData: "<tr><td>...</td></tr>", inner: true});

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
            console.log("DATA OUT", msg.payload);
            c["data_out"] = msg.payload["count"];
            break;
        case "data_in": 
            c["data_in"] = msg.payload["count"];
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

        console.log(collected);
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
                 "<td>" + render_requests(collected[keys[i]]) + "</td>" +

                 "<td>" + render_ms(collected[keys[i]].mean) + "</td>" +
                 "<td>" + render_ms(collected[keys[i]]["99"]) + "</td>" +
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
 
        let summary = _1xx_one + _2xx_one + _3xx_one + _4xx_one + _5xx_one;

        const _1_details = render_one("1xx", _1xx_one);
        const _2_details = render_one("2xx", _2xx_one);
        const _3_details = render_one("3xx", _3xx_one);
        const _4_details = render_one("4xx", _4xx_one);
        const _5_details = render_one("5xx", _5xx_one);

        return "<details><summary>" + summary + "</summary><div>"
                   + _1_details + _2_details + _3_details + _4_details + _5_details
                   + "</div></details>";
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

         return (val / 1000).toFixed(3) + " ms" 
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

            $("#usage-" + key + " .value").html(dp[key]);
            $("#usage-" + key + " .progress .progress-bar").css({width: dp[key]});
        }
         
        console.log(dp);
    }
{% endjavascript %}

{% endblock %}
