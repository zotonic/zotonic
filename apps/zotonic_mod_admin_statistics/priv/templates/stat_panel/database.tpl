{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Database _}{% endblock %}

{% block panel_body %}
<div class="row">
    <div class="col-md-6"><span class="meta">Pool Usage: </span><strong id="db-pool-usage">#</strong><span class="meta">%</span></div>
    <div class="col-md-6 text-right">
        <span id="db-pool-workers">…</span>/<span id="db-pool-working">#</span>
    </div>
</div>
<div class="progress">
    <div class="progress-bar progress-bar-success" role="progressbar" id="db-pool-usage-bar" style="width: 0%">
    </div>
</div>
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id in [
                ["Requests/min", "db-requests"],
                ["Checkouts/min", "db-checkouts"],
                ["Pool High Usage/min", "db-pool-high-usage"],
                ["Pool Full/min", "db-pool-full"]
            ]
        %}
            {% include "_stat_row.tpl" %}
        {% endfor %}
    </tbody>
</table>

{% javascript %}

cotonic.broker.subscribe("bridge/origin/$SYS/site/{{ m.site.site }}/db/+what",
    function(msg, args) {

        switch(args.what) {
        case "pool": {
                const workers = msg.payload.workers;
                const working = msg.payload.working;
                const total = workers + working;
                let usage;
                if(total > 0) {
                    usage = (working / total) * 100;
                } else {
                    usage = 0;
                }

                $("#db-pool-usage").html(usage);
                $("#db-pool-workers").html(workers);
                $("#db-pool-working").html(working);

                $("#db-pool-usage-bar").css("width", usage.toString() + "%");
            }

            break;
        case "pool_full": 
            $("#db-pool-full").html(msg.payload.one);
            break;
        case "pool_high_usage": 
            $("#db-pool-high-usage").html(msg.payload.one);
            break;
        case "connection_wait": 
            $("#db-checkouts").html(msg.payload.one);
            break;
        case "requests": 
            $("#db-requests").html(msg.payload.one);
        }
    });
{% endjavascript %}

{% endblock %}
