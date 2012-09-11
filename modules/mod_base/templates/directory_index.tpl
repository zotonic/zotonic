{% extends "base.tpl" %}

{% block title %}
{_ Index of _} {{ basename }}
{% endblock %}

{% block content %}
<h1>{_ Index of _} {{ basename }}</h1>

{% if files %}
<table>
    <thead>
        <tr>
            <th>{_ Filename _}</th>
            <th>{_ Type _}</th>
            <th>{_ Date _}</th>
            <th>{_ Size _}</th>
        </tr>
    </thead>
    <tbody>
        {% for f in files %}
        <tr>
            <td>
                <a href="{{ f.name|urlencode }}{% if f.is_dir %}/{% endif %}">{{ f.name|escape }}{% if f.is_dir %}/{% endif %}</a>
            </td>
            <td>
            {% if f.is_dir %}
            {_ Folder _}
            {% else %}
            {{ f.mime }}
            {% endif %}
            </td>
            <td>
                {{ f.last_modified|date:"Y-m-d H:i:s" }}
            </td>
            <td>
                {% if f.is_dir %}&mdash;{% else %}{{ f.size|filesizeformat }}{% endif %}
            </td>
        </tr>
        {% endfor %}
    </tbody>
</table>
{% endif %}

{% endblock %}
