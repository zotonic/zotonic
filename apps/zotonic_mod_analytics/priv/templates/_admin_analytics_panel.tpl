{% with panel.collector as c %}
<section id="analytics-recording-{{ c.id }}" class="analytics-panel {% if lead %}analytics-panel-lead{% endif %}" aria-labelledby="analytics-title-{{ c.id }}">
    <header class="analytics-panel-heading">
        <div>
            <h3 id="analytics-title-{{ c.id }}">{{ c.name|escape }}</h3>
            {% if c.state != "active" %}<span class="text-muted">{{ c.state|escape }}</span>{% endif %}
        </div>
        <div class="analytics-total"><strong>{{ panel.report.total }}</strong><span>{_ events _}</span></div>
    </header>
    {% if c.description %}<p class="text-muted">{{ c.description|escape }}</p>{% endif %}
    {% if not panel.available %}<p class="text-warning">{_ This event schema is unavailable. _}</p>{% endif %}
    {% for key, values in c.filters %}<p class="text-muted">{{ key|escape }}: {{ values|join:", "|escape }}</p>{% endfor %}
    <div class="analytics-trend">
        {% if panel.report.popular %}
            {% chart type="horizontal_bar" title=c.name data=panel.report.chart width=680 height=190 hide_table %}
        {% elseif lead %}
            {% chart type="line" title=c.name series=panel.report.chart width=1400 height=190 hide_table %}
        {% else %}
            {% chart type="line" title=c.name series=panel.report.chart width=680 height=190 hide_table %}
        {% endif %}
    </div>
    {% if panel.report.popular %}
        <details class="analytics-details">
        <summary>{_ Most popular content in this period _}</summary>
        <p class="text-muted">{_ Ranked by recorded requests in the selected moving period. The window advances in steps of _} {{ panel.report.resolution }} {_ seconds. _}</p>
        <table class="table table-condensed">
            <thead><tr><th>{_ Rank _}</th><th>{_ Content _}</th><th>{_ Requests _}</th></tr></thead>
            <tbody>{% for g in panel.report.groups|slice:[1,20] %}
                <tr><td>{{ forloop.counter }}</td><td>{{ g.label|escape }}</td><td>{{ g.count }}{% if g.upper != g.count %}–{{ g.upper }}{% endif %}</td></tr>
            {% empty %}<tr><td colspan="3">{_ No leading content identified in this period. _}</td></tr>{% endfor %}</tbody>
        </table>
        {% if panel.report.error %}<p class="text-muted">{_ Bars show the lower bound. Counts are ranges, ordered by their lower bound. Overlapping ranges may change order. Content outside the retained candidates has at most _} {{ panel.report.error }} {_ requests. _}</p>{% endif %}
        </details>
    {% elseif c.dimension %}
        <div class="analytics-breakdown">
            {% for g in panel.report.groups|slice:[1,5] %}
                <div><span>{{ g.label|escape }}</span><strong>{{ g.count }}</strong></div>
            {% empty %}<p class="text-muted">{_ No recorded events yet. _}</p>{% endfor %}
        </div>
        {% if panel.report.groups %}<details class="analytics-details">
            <summary>{_ All breakdown values _}</summary>
            <table class="table table-condensed"><thead><tr><th>{_ Breakdown _}</th><th>{_ Events _}</th></tr></thead>
                <tbody>{% for g in panel.report.groups %}<tr><td>{{ g.label|escape }}</td><td>{{ g.count }}</td></tr>{% endfor %}</tbody>
            </table>
        </details>{% endif %}
    {% endif %}
    {% if panel.report.incomplete %}<p class="analytics-coverage text-muted">{_ Includes incomplete or unrecorded intervals _}</p>{% endif %}
</section>
{% endwith %}
