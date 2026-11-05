{% if status.running %}
    <span class="label label-info">{_ Running _}</span>
{% elseif status.pending %}
    <span class="label label-warning">{_ Queued / waiting _}</span>
{% else %}
    <span class="text-muted">{_ Idle _}</span>
{% endif %}
{% if status.pending %}
    <span>{% trans "{count} background tasks remaining" count=status.pending %}</span>
{% endif %}
