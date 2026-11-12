{% extends "admin_base.tpl" %}

{% block title %}{_ Traffic guard _}{% endblock %}

{% block head_extra %}{% lib "css/traffic-guard.css" %}{% endblock %}

{% block content %}
    {% if m.acl.is_admin %}
        <div class="admin-header">
            <h2>{_ Traffic guard _}</h2>
        </div>
        <p>{_ Allow entries take precedence. Blocks follow observe/enforce/off mode and only affect guarded routes. _}</p>
        <p>{_ Current mode: _} <strong>{{ m.config.mod_traffic_guard.mode.value|default:"observe"|escape }}</strong></p>
        {% if current_ip %}
            <p>
                {_ Your current IP address: _} <code>{{ current_ip|escape }}</code>
                {% if not read_only %}
                    {% button text=_"Add to allow list" class="btn btn-default btn-sm" postback="add_current" delegate="controller_admin_traffic_guard" %}
                {% endif %}
            </p>
        {% endif %}
        {% if not read_only %}
            {% button text=_"Add IP entries" class="btn btn-primary" postback="add" delegate="controller_admin_traffic_guard" %}
        {% endif %}

        <h3>{_ IP entries _}</h3>
        <p>{_ Expiry dates are UTC. Expired entries stop applying immediately and are removed automatically. Global entries are managed in zotonic.config. _}</p>
        {% for e in entries %}
            <div class="well well-sm traffic-guard-entry {% if e.kind == "allow" %}traffic-guard-entry-allow{% else %}traffic-guard-entry-deny{% endif %}">
                <div class="traffic-guard-entry-heading">
                    <strong>{% if e.kind == "allow" %}{_ Allow _}{% else %}{_ Deny _}{% endif %} — {{ e.ip|escape }}</strong>
                    <small>{{ e.source|escape }}</small>
                    {% if e.source == "site" and not read_only %}
                        <div class="traffic-guard-entry-actions">
                            {% button text=_"Edit" class="btn btn-default btn-sm" postback={edit id=e.id} delegate="controller_admin_traffic_guard" %}
                            {% button text=_"Delete" class="btn btn-danger btn-sm"
                                action={confirm title=_"Delete IP entry?"
                                    text_template="_traffic_guard_delete_confirm.tpl"
                                    ip=e.ip note=e.note
                                    ok=_"Delete IP entry" is_danger
                                    postback={delete id=e.id} delegate="controller_admin_traffic_guard"}
                            %}
                        </div>
                    {% endif %}
                </div>
                <p class="traffic-guard-entry-note">{{ e.note|escape }}</p>
                <small>{_ Until (UTC): _} {{ e.until|default:_"Forever"|escape }}</small>
            </div>
        {% empty %}
            <p>{_ No IP entries. _}</p>
        {% endfor %}

        <h3>{_ Currently throttled IP addresses _}</h3>
        <p>{_ Current node only, including would-throttle decisions in observe mode. Up to 200 entries. Blocking generates a note from the displayed reason and rejection count. _}</p>
        <p>
            {% button text=_"Refresh" action={reload} class="btn btn-default" %}
        </p>
        {% if guard_unavailable %}
            <p class="alert alert-warning">{_ Traffic guard is temporarily unavailable. _}</p>
        {% endif %}

        <table class="table">
            <thead>
                <tr>
                    <th>{_ IP address _}</th>
                    <th>{_ Reason _}</th>
                    <th>{_ Action _}</th>
                    <th>{_ Rejections _}</th>
                    <th>{_ Retry in seconds _}</th>
                    <th>{_ Block for _}</th>
                </tr>
            </thead>
            <tbody>
                {% for t in throttled %}
                    <tr>
                        <td>{{ t.ip|escape }}</td>
                        <td>{{ t.reason|escape }}</td>
                        <td>{{ t.action|escape }}</td>
                        <td>{{ t.count }}</td>
                        <td>{{ t.retry_after }}</td>
                        <td>
                            {% if not read_only %}
                                {% with forloop.counter as block_id %}
                                    {% wire id=#block.block_id type="submit" postback="block" delegate="controller_admin_traffic_guard" %}
                                    <form id="{{ #block.block_id }}" method="post" action="postback" class="form-inline">
                                        <input type="hidden" name="ip" value="{{ t.ip|escape }}">
                                        <select name="validity" class="form-control" aria-label="{_ Block duration _}">
                                            {% include "_traffic_guard_duration.tpl" %}
                                        </select>
                                        <button type="submit" class="btn btn-danger">{_ Block IP _}</button>
                                    </form>
                                {% endwith %}
                            {% endif %}
                        </td>
                    </tr>
                {% empty %}
                    <tr>
                        <td colspan="6">{_ No currently throttled IP addresses. _}</td>
                    </tr>
                {% endfor %}
            </tbody>
        </table>
    {% endif %}
{% endblock %}
