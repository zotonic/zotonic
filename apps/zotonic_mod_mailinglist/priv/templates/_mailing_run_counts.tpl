<p>
    <strong>{{ stats.sent|default:0|format_number }}</strong> {_ sent _} ·
    {{ stats.waiting|default:0|format_number }} {_ waiting _} ·
    {{ stats.unsuccessful|default:0|format_number }} {_ could not be sent _} ·
    {{ stats.skipped|default:0|format_number }} {_ excluded _}
</p>
{% if stats.selected %}
    <progress value="{{ stats.processed }}" max="{{ stats.selected }}" aria-label="{_ Mailing progress _}"></progress>
    <span>{{ stats.processed }} / {{ stats.selected }} {_ emails processed _} ({{ stats.percent }}%)</span>
{% endif %}
