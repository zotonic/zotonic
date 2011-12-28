<h3>{{ type|statsname }}</h3>

<div class="graph" id="graph-{{ type | statskey }}"></div>
<script>
    window.graphs["{{ type | statskey }}"] = $.plot(
    $("#graph-{{ type | statskey }}"), 
    {{ stats|statsarray : type : 'second' }}
{#
,    {
    yaxis: {
    min: 0,
    max: {{ stats|statsmax }}
    },
    grid: {
    markings: [
    {% if stats.avg %}
    { color: '#33c', lineWidth: 2, yaxis: { from: {{stats.avg}}, to: {{stats.avg}} } },
    {% endif %}
    {% if stats.perc95 %}
    { color: '#c33', lineWidth: 2, yaxis: { from: {{stats.perc95}}, to: {{stats.perc95}} } }
    {% endif %}
    ]
    }
    }
#}
);
</script>

{% if stats[nodes[1]][type].perc95 %}
<p class="meta"id="stats-{{ type|statskey }}">
    {% include "_statsline.tpl" type=type stats=stats %}
</p>
{% endif %}

