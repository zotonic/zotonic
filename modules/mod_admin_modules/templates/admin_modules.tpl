{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Modules _}</h2>
        <p>{_ Zotonic is a modular web development framework. Most functionality is encapsulated inside modules. A set of basic modules are shipped with the Zotonic distribution,
            while others are externally developed. This page shows an overview of all modules which are currently known to this Zotonic installation. _}</p>
    </div>

    {% if down %}
        <div class="alert alert-warning">
            <p>
                <b><span class="glyphicon glyphicon-warning-sign"></span> {_ The following modules are not running: _}<br></b>
                {% for mod in down %}
                    {{ mod|escape }}{% if not forloop.last %},{% endif %}
                {% endfor %}
            </p>
        </div>
    {% endif %}

    <div {% include "_language_attrs.tpl" language=`en` %}>
        <table class="table table-striped do_adminLinkedTable">
            <thead>
                <tr>
                    <th width="1%"></th>
                    <th width="17%">{_ Title _}</th>
                    <th width="40%">{_ Description _}</th>
                    <th width="12%">{_ Version _}</th>
                    <th width="5%">{_ Prio _}</th>
                    <th width="25%">{_ Author _}</th>
                </tr>
            </thead>

            <tbody>
                {% for sort, prio, module, props in modules %}
                    {% with m.modules.info[module] as info %}
                    {% with configurable[module] as config_template %}
                        {% if config_template %}
                            {% wire name="dialog-"|append:module action={dialog_open template=config_template title=info.title|default:props.title|escape module=module props=props} %}
                        {% endif %}
                        <tr id="{{ #li.module }}" class="{% if not props.is_active %}unpublished{% endif %}" {% if config_template %}data-event="dialog-{{ module }}"{% endif %}>
                            <td>
                                {% include "_icon_status.tpl" status_title=status[module] status=status[module] status_id=#status.module %}
                            </td>
                            <td>
                                <strong>{{ info.title|default:props.title|escape }}</strong><br>
                                <span class="text-muted">
                                    {{ module }}
                                </span>
                            </td>
                            <td>{{ info.description|default:props.mod_description|escape|default:"-" }}</td>
                            <td>
                                {% if info.version %}
                                    <small>{{ info.version|escape }}</small>
                                {% endif %}
                            </td>
                            <td>{{ prio }}</td>
                            <td>
                                <div class="pull-right buttons">
                                    {% if props.is_active %}
                                        {% if config_template %}
                                            {% button text=_"Configure"
                                                class="btn btn-default btn-xs"
                                                action={dialog_open template=config_template title=props.mod_title|default:props.title} %}
                                        {% endif %}
                                        {% button text=_"Deactivate"
                                            class="btn btn-default btn-xs"
                                            action={module_toggle module=module status_id=#status.module}
                                            action={toggle_class id=#li.module class="enabled"} %}
                                    {% else %}
                                        {% button text=_"Activate"
                                            class="btn btn-xs btn-success"
                                            action={module_toggle module=module status_id=#status.module}
                                            action={toggle_class id=#li.module class="enabled"} %}
                                    {% endif %}
                                </div>

                                {{ props.author|escape|default:"-" }}
                            </td>
                        </tr>
                    {% endwith %}
                    {% endwith %}
                {% empty %}
                    <tr>
                        <td colspan="4">
                            {_ No modules found _}
                        </td>
                    </tr>
                {% endfor %}
            </tbody>
        </table>

    </div>
{% endblock %}
