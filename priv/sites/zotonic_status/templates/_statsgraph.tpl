<h3>{{ type|statsname }}</h3>

<div class="graph" id="graph-{{ type | statskey }}"></div>
<script>
    window.graphs["{{ type | statskey }}"] = $.plot(
    $("#graph-{{ type | statskey }}"), 
    {{ stats|statsarray : type : (q.which|default:'second') }}

,    {
    grid: {
    markings: [
    {% for n in nodes %}
    {% if stats[n][type].avg %}
    { color: '#33c', lineWidth: 1, yaxis: { from: {{stats[n][type].avg}}, to: {{stats[n][type].avg}} } },
    {% endif %}
    {% if stats[n][type].perc95 %}
    { color: '#c33', lineWidth: 1, yaxis: { from: {{stats[n][type].perc95}}, to: {{stats[n][type].perc95}} } },
    {% endif %}
    {% endfor %}
    ]
    }
    }
    {% with type as _x,_y,t %}
        {% if t == `in` or t == `out` or t == `requests` %}, stack = true{% endif %}
    {% endwith %}
);
</script>

{% if stats[nodes[1]][type].perc95 %}
<p class="meta"id="stats-{{ type|statskey }}">
    {% include "_statsline.tpl" type=type stats=stats %}
</p>
{% endif %}

