{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block head_extra %}
    {% lib "css/development.css" %}
{% endblock %}

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

            <table class="table table-condensed development-dispatch-rules">
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
                                            <li>{% if o[1] == `template` %}
                                                <span class="text-info">{_ Template _}:</span>
                                                {% include "_development_dispatch_template_option.tpl" template_option=o[2] %}
                                            {% elseif o[1] == `id` %}
                                                <span class="text-info">id:</span>
                                                {% include "_development_dispatch_id_option.tpl" resource_option=o[2] %}
                                            {% else %}{{ formatted }}{% endif %}</li>
                                        {% endif %}
                                    {% endfor %}
                                </ul>
                            </td>
                            <td class="development-dispatch-name">
                                {{ d.dispatch|escape }}
                                {% if d.dispatch_source as source %}
                                    {% include "_development_editor_button.tpl" editor_file=source.file editor_line=source.line editor_token=source.token editor_label=_"Edit dispatch file" editor_compact editor_title=_"Edit dispatch file" %}
                                {% endif %}
                            </td>
                            <td>{{ d.controller_options.zotonic_dispatch_module | escape }} </td>
                            <td>
                                {% if d.controller_source as source %}
                                    {% include "_development_editor_button.tpl" editor_file=source.file editor_line=source.line editor_token=source.token editor_label=d.controller|escape editor_inline editor_compact editor_title=_"Edit controller" %}
                                {% else %}
                                    {{ d.controller|escape }}
                                {% endif %}
                            </td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>
        </div>
    </div>
    {% endwith %}
{% else %}
    <div class="alert alert-danger">
        {_ You do not have permission to access development tools. _}
    </div>
{% endif %}

{% endblock %}
