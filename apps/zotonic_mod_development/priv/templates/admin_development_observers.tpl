{% extends "admin_base.tpl" %}

{% block title %}{_ Development - Observers _}{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Observers _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Observers _}</h2>
    <p>{_ Below is the list of all notifications and the installed observers. _}</p>
</div>

<table class="table table-striped table-hover" style="width: auto">
    <thead>
        <th>{_ Event _}</th>
        <th>{_ Subscribers _}</th>
        <th>{_ Info _}</th>
    </thead>
    <tbody>
        {% for event, observers in m.development.list_observers %}
        <tr>
            <th><tt>{{ event|escape }}</tt></th>
            <td>
                {% for prio, handler in observers %}
                    <tt>{{ handler }}</tt> &nbsp; <span class="text-muted">{{ prio }}</span><br>
                {% endfor %}
            </td>
            <td>
                <tt>
                {% with m.development.record_info[event] as recinfo %}
                    {% if recinfo %}
                        #{{ recinfo[1]|escape }}{<br>
                            {% for name, val in recinfo[2] %}
                                &nbsp;&nbsp;&nbsp;{{ name|escape }} = {{ val }}{% if not forloop.last %},{% endif %}<br>
                            {% endfor %}
                        }
                    {% else %}
                        {{ event|escape }}
                    {% endif %}
                {% endwith %}
                </tt>
            </td>
        </tr>
        {% endfor %}
    </tbody>
</table>
{% endblock %}
