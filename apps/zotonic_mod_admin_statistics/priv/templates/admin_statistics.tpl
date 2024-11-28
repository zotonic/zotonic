{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Statistics _}</h2>
    <p>{_ This page shows (real-time) system statistics like memory use and requests. _}</p>
</div>


<style type="text/css" nonce="{{ m.req.csp_nonce }}">
.stats-panels {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
}

.stats-panels > * {
    min-width: 220px;
}

.meta {
    font-weight: bold;
    color: #888;
}
</style>

<div class="stats-panels">

    {% include "stat_panel/memory_usage.tpl" %}
    {% include "stat_panel/system_usage.tpl" %}
    {% include "stat_panel/erlang.tpl" %}

    <div class="panel panel-default">
        <div class="panel-heading">IO</div>
        <div class="panel-body">
            <table class="table table-condensed">
                <thead></thead>
                <tbody>
                    {% for title, id, format in [
                            ["Input", "io-input", "filesize"],
                            ["Output", "io-output", "filesize"]
                        ]
                    %}
                        {% include "_stat_row.tpl" %}
                    {% endfor %}
                </tbody>
            </table>
        </div>
    </div>

    {% include "stat_panel/broker.tpl" %}
    {% include "stat_panel/database.tpl" %}
    {% include "stat_panel/filezcache.tpl" %}

    {% include "stat_panel/dispatch.tpl" %}

</div>

{% javascript %}
function format_view(format, value, per) {
    switch (format) {
        case "filesize":
            return render_filesize(value, per);
        case "msec":
            return render_ms(value);
        default:
            return escape(render_value(value));
    }
}

function render_filesize(value, per) {
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

function render_value(val) {
    if(!val) return "-";
    return val.toString();
}

function render_ms(val) {
    if(!val) return "-";

    return (val / 1000).toFixed(3) + "<small class=\"meta\">ms</small>"
}

function update_values(entry, payload) {
    const datapoints = Object.keys(payload);

    for(let i=0; i < datapoints.length; i++) {
        const itemId = "#" + entry + "-" + datapoints[i];
        const $item = $(itemId);

        if($item.length > 0) {
            const formatted = format_view($item.attr('data-format'), payload[datapoints[i]]);
            $item.html(formatted);
        } else {
            console.log("No place for", itemId, payload[datapoints[i]]);
        }
    }
}

function update_usage(dp) {
    const keys = Object.keys(dp);
    for(let i=0; i < keys.length; i++) {
        const key = keys[i];

        $("#usage-" + key + "-value").html(dp[key]);
        $("#usage-" + key + "-progress").css({width: dp[key].toString() + "%"});
    }
}

cotonic.broker.subscribe("bridge/origin/$SYS/erlang/+entry",
     function(msg, args) {
        if(args.entry === "usage") {
            update_usage(msg.payload);
        } else {
            update_values(args.entry, msg.payload);
        }
    });

cotonic.broker.subscribe("bridge/origin/$SYS/statistics/+entry",
     function(msg, args) {
        update_values(args.entry, msg.payload);
    });

{% endjavascript %}

{% endblock %}
