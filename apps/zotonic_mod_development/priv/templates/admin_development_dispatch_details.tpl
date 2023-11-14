{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ View all dispatch rules _}</li>
</ul>

<div class="admin-header">
    <h2>{_ View all dispatch rules available in the site _}</h2>
    <p>{_ Every URL in the site is TODO.. _}</p>
</div>

{% with m.development.dispatch_info as info %}

<dl class="dl-horizontal">
    <dt>Site</dt>
    <dl>{{ info.site | pprint }}</dl>

    <dt>Hostname</dt>
    <dl>{{ info.hostname | escape }}</dl>

    {% if info.hostalias %}
        <dt>Hostalias</dt>
        <dl>{% for i in info.hostalias %}{{ i | escape }}{% endfor %}</dl>
        <dt>Redirect</dt>
        <dl>{% if info.is_redirect %}{_ Yes _}{% else %}{_ No _}{% endif %}</dl>
    {% endif %}
</dl>

<table class="table table-condensed">
    <thead>
        <tr>
            <th>{_ URI Pattern _}</th>
            <th>{_ Options _}</th>
            <th>{_ Module _}</th>
            <th>{_ Dispatch Name _}</th>
            <th>{_ Controller _}</th>
        </tr>
    </thead>
    <tbody>

{% for d in info.dispatch_list %}
<tr>
    <td>
        <ol class="breadcrumb" style="padding: 0px; background-color: inherit; margin-bottom: inherit;">
            <li class="breadcrumb-item"></li>
            {% for elt in d.path %}
                <li class="breadcrumb-item">{{ elt | format_dispatch_path_element }}</li>
            {% empty %}
                <li class="breadcrumb-item">&nbsp;</li>
            {% endfor %}
        </ol>
    </td>
    <td>
        <ul class="list-inline">
         {% for o in d.controller_options  %}
             {% if o | format_dispatch_controller_option:d.controller as formatted %}
                 <li>{{ formatted }}</li>
             {% endif %}
         {% endfor %}
        </ul>
    </td>
    <td>{{ d.controller_options.zotonic_dispatch_module | escape }} </td>
    <td>{{ d.dispatch | escape }} </td>
    <td>{{ d.controller | escape }} </td>
</tr>
{% endfor %}

</table>


{% endwith %}

{% endblock %}


