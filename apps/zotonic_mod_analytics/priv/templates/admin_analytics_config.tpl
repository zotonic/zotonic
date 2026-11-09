{% extends "admin_base.tpl" %}
{% block title %}{_ Analytics configuration _}{% endblock %}
{% block head_extra %}{% lib "css/analytics.css" %}{% endblock %}
{% block content %}
{% if m.acl.is_admin %}
<div class="admin-header analytics-heading">
    <div><h2>{_ Analytics configuration _}</h2><p>{_ Choose which activity to measure and arrange your dashboard. _}</p></div>
    {% if m.acl.use.mod_analytics %}<a class="btn btn-default" href="{% url admin_analytics %}">{_ View analytics _}</a>{% endif %}
</div>
{% with analytics|default:m.analytics.configuration as a %}
<div class="widget">
    <h3 class="widget-header">{_ Analytics measurements _}</h3>
    <div class="widget-content">
        <p>{_ Drag analytics measurements to set their order on the dashboard. Changes are saved immediately. _}</p>
        {% sorter id="analytics-recordings" tag="recordings" delegate="mod_analytics" handle=".analytics-drag" axis="y" %}
        <table class="table">
            <thead><tr><th><span class="sr-only">{_ Display order _}</span></th><th>{_ Name _}</th><th>{_ Event _}</th><th>{_ Breakdown _}</th><th>{_ Group limit _}</th><th>{_ State _}</th><th></th></tr></thead>
            <tbody id="analytics-recordings">{% for c in a.recordings %}
                {% with c.id as recording_id %}
                {% sortable id=#recording.recording_id tag=c.id %}
                <tr id="{{ #recording.recording_id }}"><td class="analytics-drag" title="{_ Drag to reorder _}"><span aria-hidden="true">↕</span><span class="sr-only">{_ Drag to reorder _}</span></td><td>{{ c.name|escape }}{% if c.description %}<p class="text-muted">{{ c.description|escape }}</p>{% endif %}</td><td>{{ c.event|escape }}</td><td>{{ c.dimension|default:_"Total"|escape }}</td><td>{{ c.groups }}</td><td>{{ c.state|escape }}</td>
                    <td>{% if m.acl.is_admin %}
                        {% button text=_"Info" class="btn btn-default btn-xs" action={dialog_open title=_"Analytics measurement information" template="_dialog_analytics_info.tpl" recording_id=c.id} %}
                        {% if c.state == "active" %}{% button text=_"Pause" class="btn btn-default btn-xs" postback={state id=c.id state="paused"} delegate="mod_analytics" %}
                        {% else %}{% button text=_"Start analytics measurement" class="btn btn-primary btn-xs" postback={state id=c.id state="active"} delegate="mod_analytics" %}{% endif %}
                        {% button text=_"Delete" class="btn btn-danger btn-xs"
                            action={confirm title=_"Delete analytics measurement?"
                                text_template="_admin_analytics_delete_confirm.tpl"
                                recording_name=c.name
                                ok=_"Delete analytics measurement" is_danger
                                postback={delete_recording id=c.id} delegate="mod_analytics"} %}
                    {% endif %}</td>
                </tr>
                {% endwith %}
            {% endfor %}</tbody>
        </table>
    </div>
</div>
<div class="analytics-recording-actions">
{% button text=_"Add analytics measurement" class="btn btn-primary" action={dialog_open title=_"Add analytics measurement" template="_dialog_analytics_recording.tpl"} %}
</div>
{% include "_admin_analytics_status.tpl" %}
{% endwith %}
{% endif %}
{% endblock %}
