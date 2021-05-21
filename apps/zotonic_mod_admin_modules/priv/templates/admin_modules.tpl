{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Modules _}</h2>
        <p style="max-width: 100ch;">{_ Zotonic is a modular web development framework. Most functionality is encapsulated inside modules. A set of basic modules are shipped with the Zotonic distribution,
            while others are externally developed. This page shows an overview of all modules which are currently known to this Zotonic installation. _}</p>
    </div>

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th></th>
                <th>{_ Module _}</th>
                <th>{_ Depends _}</th>
                <th>{_ Provides _}</th>
                {% comment %}<th>{_ Prio _}</th>{% endcomment %}
            </tr>
        </thead>

        <tbody>
        {% with m.modules.provided as active_provided %}
        {% with m.modules.get_provided as provided %}
        {% with m.modules.get_depending as depending %}
            {% for sort, prio, module, props in modules %}
                {% with configurable[module] as config_template %}
                    {% if config_template %}
                        {% wire name="dialog-"|append:module action={dialog_open template=config_template title=props.mod_title|default:props.title module=module props=props} %}
                    {% endif %}
                    <tr class="{% if not props.is_active %}unpublished{% endif %}" {% if config_template %}data-event="dialog-{{ module }}"{% endif %}>
                        <td>
                            {% include "_icon_status.tpl" status_title=status[module] status=status[module] status_id=#status.module %}
                        </td>
                        <td>
                            <strong>{{ props.mod_title|default:props.title }}</strong><br>
                            {{ props.mod_description|default:"-" }}<br>
                        </td>
                        <td>
                            {% for m in props.mod_depends %}
                                {% if m|member:active_provided %}
                                    <nobr><span class="text-success">&check;</span> {{ m }}</nobr>
                                {% else %}
                                    <nobr><span class="text-danger">&times; {{ m }}</span></nobr>
                                {% endif %}
                                {% if not forloop.last %}<br>{% endif %}
                            {% endfor %}
                        </td>
                        <td>
                            {% if depending[module] %}
                                <nobr><span class="text-success">&ofcir;</span> {{ module }}</span></nobr>
                            {% else %}
                                <nobr><span class="text-muted">&cir; {{ module }}</span></nobr>
                            {% endif %}
                            {% for m in props.mod_provides %}
                                {% if m /= module %}
                                    <br>
                                    {% if depending[m] %}
                                        <nobr><span class="text-success">&ofcir;</span> {{ m }}</nobr>
                                    {% else %}
                                        <nobr><span class="text-muted">&cir; {{ m }}</span></nobr>
                                    {% endif %}
                                {% endif %}
                            {% endfor %}
                        </td>
                        {% comment %}<td>{{ prio }}</td>{% endcomment %}
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
                                        action={module_toggle is_deactivate module=module}
                                    %}
                                {% else %}
                                    {% button text=_"Activate"
                                        class="btn btn-info btn-xs"
                                        action={module_toggle is_activate module=module}
                                    %}
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
{% endblock %}
