{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Modules _}</h2>
    <p>{_ Zotonic is a modular web development framework. Most functionality is encapsulated inside modules. A set of basic modules are shipped with the Zotonic distribution,
    while others are externally developed. This page shows an overview of all modules which are currently known to this Zotonic installation. _}</p>
</div>

<div {% include "_language_attrs.tpl" language=`en` %}>
    <table class="table table-striped">
        <thead>
            <tr>
                <th width="20%">{_ Title _}</th>
                <th width="45%">{_ Description _}</th>
                <th width="5%">{_ Prio _}</th>
                <th width="30%">{_ Author _}</th>
            </tr>
        </thead>
        
        <tbody>
            {% for sort, prio, module, props in modules %}
            <tr id="{{ #li.module }}" class="{% if not props.is_active %}unpublished{% endif %}">
                <td>{% include "_icon_status.tpl" status=status[module] status_id=#status.module %} {{ props.mod_title|default:props.title }}</td>
                <td>{{ props.mod_description|default:"-" }}</td>
                <td>{{ prio }}</td>
                <td>
                    <div class="pull-right">
                        {% if props.is_active %}
                        {% button text=_"Deactivate"
                           class="btn btn-mini"
                           action={module_toggle module=module status_id=#status.module}
                           action={toggle_class id=#li.module class="enabled"} %}
                        {% else %}
                        {% button text=_"Activate"
                           class="btn btn-mini btn-success"
                           action={module_toggle module=module status_id=#status.module} 
                           action={toggle_class id=#li.module class="enabled"} %}
                        {% endif %}
                    </div>

                    {{ props.author|escape|default:"-" }}
                </td>
            </tr>
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
