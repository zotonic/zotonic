<h3>{{ type|statsname }}</h3>

<div class="graph" id="graph-{{ type | statskey }}"></div>
<script>
    window.graphs["{{ type | statskey }}"] = $.plot(
    $("#graph-{{ type | statskey }}"), 
    {{ stats|statsarray : type : 'second' }}

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
);
</script>

{% if stats[nodes[1]][type].perc95 %}
<p class="meta"id="stats-{{ type|statskey }}">
    {% include "_statsline.tpl" type=type stats=stats %}
</p>
{% endif %}

