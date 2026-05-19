{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log CSP reports _}{% endblock %}

{% block title_log %}{_ Content-Security-Policy reports _}{% endblock %}

{% block active3 %}active{% endblock %}

{% block content_log %}

<h3 class="above-list">
    {_ Most recently reported Content-Security-Policy violations. _}
</h3>

<p class="help-block">
    {_ The latest 100 reported directives and blocked contents are kept. For each, the last 10 reported source locations are shown.  _}
</p>

<div class="alert alert-danger">
    {_ Content-Security-Policy violations are reported to an open endpoint and can contain spam. _}<br>
</div>

{% with m.log_csp.reports as reports %}
<table class="table table-compact">
    <thead>
        <tr>
            <th>{_ Date _}</th>
            <th>{_ Directive _}</th>
            <th>{_ Blocked _}</th>
            <th>{_ Count _}</th>
            <th>{_ Reporting URLs _}</th>
            <th>{_ Sources _}</th>
            <th>{_ User-Agents _}</th>
        </tr>
    </thead>
    <tbody>
        {% for r in reports  %}
            <tr>
                <td>{{ r.date|date:"Y-m-d H:i:s" }}</td>
                <td>{{ r.effective_directive|escape }}</td>
                <td>{{ r.blocked_url|escape }}</td>
                <td>{{ r.count }}</td>
                <td>
                    {% for url in r.reporting_url %}
                        <a href="{{ url|escape }}" title="{{ url|escape }}" target="_blank">{{ url|truncatechars:50|escape }} <span class="fa fa-external-link"></span></a><br>
                    {% endfor %}
                </td>
                <td>
                    {% for s in r.source %}
                        {{ s.file|truncatechars:80|escape }}<span class="text-muted">:{{ s.line }}:{{ s.column }}</span><br>
                    {% endfor %}
                </td>
                <td>
                    {% for s in r.user_agent %}
                        {{ s|truncatechars:400|escape }}<br>
                    {% endfor %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="7" class="text-muted">{_ No Content-Security-Policy reports. _}</td>
            </tr>
        {% endfor %}
    </tbody>
</table>

{% if reports %}
<div class="well">
    <p>{_ Latest reported security policy: _}</p>
    <pre>{{ reports[1].original_policy|trim|replace:"; ":";"|replace:";":";\n"|escape|linebreaksbr }}</pre>
</div>
{% endif %}

{% endwith %}

{% endblock %}
