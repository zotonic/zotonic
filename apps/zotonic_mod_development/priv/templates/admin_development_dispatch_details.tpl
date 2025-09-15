{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ View all dispatch rules _}</li>
</ul>

<div class="admin-header">
    <h2>{_ View all dispatch rules available in the site _}</h2>

    <p>{_ Every URL which is reachable in the site is defined with dispatch rules. This page shows all available dispatch rules in this site. _}</p>
    <p>{% trans "For more information see: { url }"
                url="<a target='_blank' href='https://zotonic.com/docs/doc_developerguide_dispatch_rules'>Dispatch rules</a>" %}
</div>


{% if m.acl.is_allowed.use.mod_development %}
    {% with m.development.dispatch_info as info %}
    <div class="widget">
        <div class="widget-header">
            {_ Site Dispatch Rules _}
        </div>

        <div class="widget-content">
            <dl class="dl-horizontal">
                <dt class="text-muted">{_ Site Name _}</dt>
                <dd>{{ info.site | escape }}</dd>

                <dt class="text-muted">{_ Hostname _}</dt>
                <dd>{{ info.hostname | escape }}</dd>

                {% if info.hostalias %}
                <dt class="text-muted">{_ Hostalias _}</dt>
                    <dd>{% for a in info.hostalias %}{{ a | escape }}{% if not forloop.last %}, {% endif %}{% endfor %}</dd>

                    <dt class="text-muted">Redirect Enabled?</dt>
                    <dd>{% if info.is_redirect %}{_ Yes _}{% else %}{_ No _}{% endif %}</dd>
                {% endif %}
            </dl>

            <table class="table table-condensed">
                <thead>
                    <tr>
                        <th style="width: 30%">{_ URI Pattern _}</th>
                        <th>{_ Options _}</th>
                        <th>{_ Dispatch Name _}</th>
                        <th>{_ Module _}</th>
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
                            <td>{{ d.dispatch | escape }} </td>
                            <td>{{ d.controller_options.zotonic_dispatch_module | escape }} </td>
                            <td>{{ d.controller | escape }} </td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>
        </div>
    </div>
{% else %}
    <div class="alert alert-danger">
        {_ You do not have permission to access development tools. _}
    </div>
{% endif %}

{% endwith %}
{% endblock %}
