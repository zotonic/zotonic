{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Modules _}</h2>
        <p>{_ Zotonic is a modular web development framework. Most functionality is encapsulated inside modules. A set of basic modules are shipped with the Zotonic distribution,
            while others are externally developed. This page shows an overview of all modules which are currently known to this Zotonic installation. _}</p>
    </div>

    <div {% include "_language_attrs.tpl" language=`en` %}>
        <table class="table table-striped do_adminLinkedTable">
            <thead>
                <tr>
                    <th></th>
                    <th>{_ Module _}</th>
                    <th>{_ Provides _}</th>
                    <th>{_ Depends _}</th>
                    <th>{_ Prio _}</th>
                </tr>
            </thead>

            <tbody>
            {% with m.modules.provided as current_provided %}
            {% with m.modules.get_provided as provided %}
            {% with m.modules.get_depending as depending %}
                {% for sort, prio, module, props in modules %}
                    {% with configurable[module] as config_template %}
                        {% if config_template %}
                            {% wire name="dialog-"|append:module action={dialog_open template=config_template title=props.mod_title|default:props.title module=module props=props} %}
                        {% endif %}
                        <tr id="{{ #li.module }}" class="{% if not props.is_active %}unpublished{% endif %}" {% if config_template %}data-event="dialog-{{ module }}"{% endif %}>
                            <td>
                                {% include "_icon_status.tpl" status_title=status[module] status=status[module] status_id=#status.module %}
                            </td>
                            <td>
                                <strong>{{ props.mod_title|default:props.title }}</strong><br>
                                {{ props.mod_description|default:"-" }}<br>
                            </td>
                            <td>
                                <nobr>{{ module }}</nobr>
                                {% for m in props.mod_provides %}
                                    {% if m /= module %}
                                        <br><nobr><span class="text-muted">{{ m }}</span></nobr>
                                    {% endif %}
                                {% endfor %}
                            </td>
                            <td>
                                {% for m in props.mod_depends %}
                                    {% if m|member:current_provided %}
                                        <nobr><span class="text-success">√</span> {{ m }}</nobr>
                                    {% else %}
                                        <nobr><span class="text-danger">x</span> {{ m }}</nobr>
                                    {% endif %}
                                    {% if not forloop.last %}<br>{% endif %}
                                {% endfor %}
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
                                            action={toggle_class target=#li.module class="unpublished"} %}
                                    {% else %}
                                        {% button text=_"Activate"
                                            class="btn btn-info btn-xs"
                                            action={module_toggle module=module status_id=#status.module}
                                            action={toggle_class target=#li.module class="unpublished"} %}
                                    {% endif %}
                                </div>
                            </td>
                        </tr>
                    {% endwith %}
                {% empty %}
                    <tr>
                        <td colspan="4">
                            {_ No modules found _}
                        </td>
                    </tr>
                {% endfor %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
            </tbody>
        </table>

    </div>
{% endblock %}
