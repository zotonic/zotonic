{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Broker _}{% endblock %}

{% block panel_body %}
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id in [ ["Sessions", "broker-session_count"],
        ["Subscribes/min", "broker-subscribe_one"],
        ["Publishes/min", "broker-publish_one"],
        ["Subscribers", "broker-destinations"],
        ["Details", ""],
        ["Nodes", "broker-nodes"],
        ["Edges", "broker-edges"],
        ["Wildcards", "broker-wildcards"],
        ["Paths", "broker-paths"] ] %}
        {% include "_stat_row.tpl" %}
        {% endfor %}
    </tbody>
</table>

{% javascript %}
cotonic.broker.subscribe("bridge/origin/$SYS/site/blog/broker/+what",
    function(msg, args) {
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
    });
{% endjavascript %}

{% endblock %}
