<p>
    {% trans "Sent: <strong>{count}</strong>" count=stats.sent|default:0|format_number %} ·
    {% trans "Waiting: {count}" count=stats.waiting|default:0|format_number %} ·
    {% trans "Could not be sent: {count}" count=stats.unsuccessful|default:0|format_number %} ·
    {% trans "Excluded: {count}" count=stats.skipped|default:0|format_number %}
</p>
{% if stats.selected %}
    <progress value="{{ stats.processed }}" max="{{ stats.selected }}" aria-label="{_ Mailing progress _}"></progress>
    <span>{% trans "Emails processed: {processed} / {selected}" processed=stats.processed selected=stats.selected %} ({{ stats.percent }}%)</span>
{% endif %}
