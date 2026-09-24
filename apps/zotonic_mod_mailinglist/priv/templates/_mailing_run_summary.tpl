<dl class="mailing-run-metrics">
    <div><dt>{_ Sent _}</dt><dd>{{ stats.sent|default:0|format_number }}</dd></div>
    <div><dt>{_ Waiting _}</dt><dd>{{ stats.waiting|default:0|format_number }}</dd></div>
    <div><dt>{_ Could not be sent _}</dt><dd>{{ stats.unsuccessful|default:0|format_number }}</dd></div>
    <div><dt>{_ Excluded _}</dt><dd>{{ stats.skipped|default:0|format_number }}</dd></div>
</dl>
{% if stats.selected %}
<div class="mailing-run-progress">
    <progress value="{{ stats.processed }}" max="{{ stats.selected }}" aria-label="{_ Mailing progress _}"></progress>
    <span>{{ stats.processed|format_number }} / {{ stats.selected|format_number }} {_ emails processed _} <strong>{{ stats.percent }}%</strong></span>
</div>
{% endif %}

<p class="help-block">{_ Sent means accepted by the mail server, not confirmed inbox delivery. Later bounces update these results. _}</p>
