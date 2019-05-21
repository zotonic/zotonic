{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log ui events _}{% endblock %}

{% block title_log %}{_ Log user interface events _}{% endblock %}

{% block active3 %}active{% endblock %}

{% block content_log %}

<h3 class="above-list">
    {_ Most recent user interface events _}
</h3>
<br />

<form id="log_filter" action="" type="GET">
{% with m.search[{log_ui page=q.page type=q.type user=q.user pagelen=100}] as result %}
    <table class="table table-compact">
        <tr>
            <th>{_ Date _}</th>
            <th>{_ Severity _}</th>
            <th>{_ Message _}</th>
            <th>{_ User _}</th>
            <th>{_ Stack _}</th>
        </tr>
        <tr>
            <td></td>
            <td>
                <select name="type" id="log_severity" class="form-control">
                    <option {% if q.type == 'debug' %}selected{% endif %} value="debug">Debug</option>
                    <option {% if q.type == 'info' %}selected{% endif %} value="info">Info</option>
                    <option {% if not q.type or q.type == 'warning' %}selected{% endif %} value="warning">Warning</option>
                    <option {% if q.type == 'error' %}selected{% endif %} value="error">Error</option>
                </select>
                {% wire id="log_severity" type="change" action={submit target="log_filter"} %}
            </td>
            <td>
                <input type="text" id="log_message" name="message" placeholder="{_ Message _}" class="form-control" value="{{ q.message|escape }}">
            </td>
            <td>
                <input type="number" id="log_user" name="user" min="1" placeholder="{_ User Id _}" class="form-control" value="{{ q.user|escape }}">
            </td>
            <td>
                <button type="submit" class="btn btn-primary">{_ Filter _}</button>
                <button class="btn btn-default" id="filter_clear">{_ All _}</button>
                {% wire id="filter_clear"
                    action={set_value selector="#log_message input" value=""}
                    action={set_value selector="#log_user" value=""}
                    action={set_value selector="#log_severity" value="debug"}
                    action={submit target="log_filter"}
                %}
            </td>
        </tr>
        <tbody id="log-area">
            {% for id in result %}
                {% include "_admin_log_ui_row.tpl" id=id qmessage=q.message %}
            {% empty %}
                <tr>
                   <td colspan="5" class="text-muted">{_ No log messages. _}</td>
                </tr>
            {% endfor %}
        </tbody>
    </table>
    {% lazy action={moreresults result=result
                                target="log-area"
                                template="_admin_log_ui_row.tpl"
                                qmessage=q.message
                                visible}
    %}
{% endwith %}
</form>

{% endblock %}
