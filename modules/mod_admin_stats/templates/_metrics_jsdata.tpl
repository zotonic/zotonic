[
{% for metric in m.stats.metrics %}
    {
    type: "{{ metric.type }}",
    system: "{{ metric.system }}",
    name: "{{ metric.name }}",
    {% if metric.type == `histogram` %}
        histogram: [
        {% for bin, count in metric.value.histogram %}
            {x: "{{ bin/1000 }}", y: {{ count }} },
        {% endfor %}
        ],
        min: {{ metric.value.min/1000 }},
        max: {{ metric.value.max/1000 }},
        mean: {
        arithmetic: {{ metric.value.arithmetic_mean }},
        geometric: {{ metric.value.geometric_mean }},
        harmonic: {{ metric.value.harmonic_mean }}
        }
    {% endif %}
    },
{% endfor %}
]
