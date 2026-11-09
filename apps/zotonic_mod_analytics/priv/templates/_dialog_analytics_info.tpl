{% if m.acl.is_admin %}
{% with m.analytics.info[recording_id] as info %}
{% if info %}
{% with info.collector as c %}
<h3>{{ c.name|escape }}</h3>
{% if c.description %}<p>{{ c.description|escape }}</p>{% endif %}
<dl class="dl-horizontal">
    <dt>{_ Recording ID _}</dt><dd>{{ c.id }}</dd>
    <dt>{_ State _}</dt><dd>{{ c.state|escape }}</dd>
    <dt>{_ Analytics event type _}</dt><dd>{{ c.event|escape }}</dd>
    <dt>{_ Schema version _}</dt><dd>{{ c.schema_version }}</dd>
    <dt>{_ Break down by _}</dt><dd>{{ c.dimension|default:_"No breakdown (total)"|escape }}</dd>
    <dt>{_ Group limit _}</dt><dd>{{ c.groups }}</dd>
    <dt>{_ Created _}</dt><dd>{{ info.created|date:"Y-m-d H:i:s T" }}</dd>
    <dt>{_ Started _}</dt><dd>{{ info.started|date:"Y-m-d H:i:s T" }}</dd>
    <dt>{_ Fixed filters _}</dt><dd>{% for key, values in c.filters %}{{ key|escape }} = {{ values|join:", "|escape }}{% if not forloop.last %}<br>{% endif %}{% empty %}{_ None — all matching events _}{% endfor %}</dd>
</dl>
<h4>{_ Storage usage _}</h4>
<dl class="dl-horizontal">
    <dt>{_ Stored data size _}</dt><dd>{{ info.payload_bytes|filesizeformat }} ({{ info.payload_bytes }} {_ bytes _})</dd>
    <dt>{_ Stored time intervals _}</dt><dd>{{ info.buckets }}</dd>
    <dt>{_ Reserved capacity _}</dt><dd>{{ info.reserved }} {_ group/time slots _}</dd>
</dl>
<p class="help-block">{_ Stored data size measures the database space used by the aggregate counts and recording definition. It excludes row headers, indexes, shared table space and unsaved events. Reserved capacity is a planning limit, not disk usage. _}</p>
<div class="table-responsive">
<table class="table table-condensed">
    <thead><tr><th>{_ Detail _}</th><th>{_ Stored intervals / capacity _}</th><th>{_ Data size _}</th><th>{_ Oldest stored interval _}</th><th>{_ Newest stored interval _}</th></tr></thead>
    <tbody>{% for t in info.tiers %}<tr>
        <td>{% if t.resolution == 60 %}{_ 1 minute _}{% elseif t.resolution == 900 %}{_ 15 minutes _}{% elseif t.resolution == 3600 %}{_ 1 hour _}{% else %}{_ 1 day _}{% endif %}</td>
        <td>{{ t.buckets }} / {{ t.slots }}</td><td>{{ t.bytes|filesizeformat }}</td>
        <td>{% if t.oldest %}{{ t.oldest|date:"Y-m-d H:i T" }}{% else %}—{% endif %}</td>
        <td>{% if t.newest %}{{ t.newest|date:"Y-m-d H:i T" }}{% else %}—{% endif %}</td>
    </tr>{% endfor %}</tbody>
</table>
</div>
<p class="help-block">{_ Dates show the start of each stored time interval, not the first or last event. Different resolutions contain overlapping history. Pausing a recording preserves its stored history and reserved capacity. Figures are a snapshot taken when this dialog opens. _}</p>
{% endwith %}
{% else %}<p>{_ This recording is no longer available. _}</p>{% endif %}
{% endwith %}
<div class="modal-footer">{% button text=_"Close" class="btn btn-default" action={dialog_close} %}</div>
{% endif %}
