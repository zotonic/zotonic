{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log messages _}{% endblock %}

{% block title_log %}{_ Log messages _}{% endblock %}

{% block active1 %}active{% endblock %}

{% block content_log %}

<h3 class="above-list">
    {_ Most recent messages _}
</h3>
<br />

<form action="" type="GET">
{% with m.search[{log page=q.page type=q.type user=q.user pagelen=100}] as result %}
    <table class="table table-compact">
        <tr>
            <th>{_ Date _}</th>
            <th>{_ Severity _}</th>
            <th>{_ Message _}</th>
            <th>{_ User _}</th>
            <th>{_ Module _}</th>
        </tr>
        <tr>
            <td></td>
            <td>
                <select name="type" class="form-control">
                    <option {% if q.type == 'debug' %}selected{% endif %} value="debug">Debug</option>
                    <option {% if q.type == 'info' %}selected{% endif %} value="info">Info</option>
                    <option {% if not q.type or q.type == 'warning' %}selected{% endif %} value="warning">Warning</option>
                    <option {% if q.type == 'error' %}selected{% endif %} value="error">Error</option>
                </select>
            </td>
            <td>
                <input type="text" name="message" placeholder="{_ Message _}" class="form-control" value="{{ q.message|escape }}">
            </td>
            <td>
                <input type="number" name="user" min="1" placeholder="{_ User Id _}" class="form-control" value="{{ q.user|escape }}">
            </td>
            <td>
                <button type="submit" class="btn btn-primary">{_ Filter _}</button>
            </td>
        </tr>
        <tbody id="log-area">
            {% for id in result %}
                {% include "_admin_log_row.tpl" id=id qmessage=q.message %}
            {% empty %}
                <tr>
            	   <td colspan="5" class="text-muted">{_ No log messages. _}</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
    {% lazy action={moreresults result=result
                                target="log-area"
                                template="_admin_log_row.tpl"
                                qmessage=q.message
                                visible}
    %}
{% endwith %}
</form>

{% wire action={connect signal={log_message} action={addlog target="log-area" quser=q.user qmessage=q.message}} %}

{% endblock %}
