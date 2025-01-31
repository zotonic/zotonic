{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Modules _}</h2>
        <p style="max-width: 100ch;">{_ Zotonic is a modular web development framework. Most functionality is encapsulated inside modules. A set of basic modules are shipped with the Zotonic distribution,
            while others are externally developed. This page shows an overview of all modules which are currently known to this Zotonic installation. _}</p>
    </div>

    {% if m.modules.uninstalled as uninstalled %}
        <div class="well">
            <p>
                <span class="glyphicon glyphicon-exclamation-sign"></span>
                {_ The modules below are activated but could not be found. _}
                {_ Either they are not installed, or they are not compiled. _}
                <br>
                {_ Deactivate these modules if you are not using them, or check the installation of the system. _}
                <br>
            </p>

            <table class="table table-striped">
                <tr>
                    <th width="20"></th>
                    <th>{_ Module _}</th>
                    <th></th>
                </tr>
                {% for module in uninstalled %}
                    <tr class="clickable">
                        <td>{% include "_icon_status.tpl" status=undefined %}
                        <td>{{ module|escape }}</td>
                        <td>
                            {% button text=_"Deactivate"
                                class="btn btn-default btn-xs"
                                action={module_toggle is_deactivate module=module}
                            %}
                        </td>
                    </tr>
                {% endfor%}
            </table>
        </div>
    {% endif %}

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20"></th>
                <th>{_ Module _}</th>
                <th>{_ Depends _}</th>
                <th>{_ Provides _}</th>
                <th></th>
            </tr>
        </thead>

        <tbody>
        {% with m.modules.provided as active_provided %}
        {% with m.modules.get_provided as provided %}
        {% with m.modules.get_depending as depending %}
            {% for sort, prio, module, props in modules %}
                {% with m.modules.info[module] as info %}
                {% with configurable[module] as config_template %}
                    {% if config_template %}
                        {% wire name="dialog-"|append:module action={dialog_open template=config_template title=info.title|default:props.title|escape module=module props=props} %}
                    {% endif %}
                    <tr class="{% if not props.is_active %}unpublished{% endif %} clickable" {% if config_template %}data-event="dialog-{{ module }}"{% endif %}>
                        <td>
                            {% include "_icon_status.tpl" status_title=status[module] status=status[module] status_id=#status.module %}
                        </td>
                        <td>
                            <strong>{{ info.title|default:props.title|escape }}</strong>
                            <small class="text-muted">&nbsp; {{ info.version|escape }}</small>
                            <br>{{ info.description|escape }}
                        </td>
                        <td>
                            {% for d in props.mod_depends %}
                                {% if d|member:active_provided %}
                                    <nobr><span class="text-success">&check;</span> {{ d }}</nobr>
                                {% else %}
                                    <nobr><span class="text-danger">&times; {{ d }}</span></nobr>
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
                            {% for p in props.mod_provides %}
                                {% if p /= module %}
                                    <br>
                                    {% if depending[p] %}
                                        <nobr><span class="text-success">&ofcir;</span> {{ p }}</nobr>
                                    {% else %}
                                        <nobr><span class="text-muted">&cir; {{ p }}</span></nobr>
                                    {% endif %}
                                {% endif %}
                            {% endfor %}
                        </td>
                        <td>
                            <div class="buttons" style="white-space: nowrap; text-align: right">
                                {% if props.schema %}
                                    {% button text=_"Reinstall"
                                        title=_"Install the module’s model and data."
                                        class="btn btn-xs btn-default"
                                        postback={reinstall module=module}
                                        delegate=`controller_admin_module_manager`
                                    %}
                                {% endif %}

                                {% if props.is_active %}
                                    {% if config_template %}
                                        {% button text=_"Configure"
                                            class="btn btn-default btn-xs"
                                            action={dialog_open template=config_template title=props.mod_title|default:props.title} %}
                                    {% endif %}
                                    {% button text=_"Deactivate"
                                        class="btn btn-warning btn-xs"
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
