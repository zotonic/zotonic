{#
Param:
id
#}
{% if id.date_end|in_past %}
    <span class="text-muted">{{ id.date_start|timesince }}</span>
{% else %}
    <span>{{ id.date_start|timesince }}</span>
{% endif %}