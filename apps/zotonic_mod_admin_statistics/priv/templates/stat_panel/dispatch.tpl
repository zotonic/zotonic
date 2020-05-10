{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Dispatch _}{% endblock %}

{% block panel_body %}
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

{% javascript %}

let collected = {};

cotonic.broker.publish("model/ui/insert/the-stats", {initialData: "<tr><td>...</td></tr>", inner: true});

cotonic.broker.subscribe("bridge/origin/$SYS/site/blog/cowmachine/+dispatch/+what",
    function(msg, args) {
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

{% endjavascript %}

{% endblock %}
