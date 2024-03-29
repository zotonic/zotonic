{% extends "admin_base.tpl" %}

{% block title %}{_ ACL rules _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Access control rules _}</h2>
</div>

{% if m.acl.use.mod_acl_user_groups %}
    {% with m.acl.insert.acl_user_group as is_editable %}

        {% if is_editable %}
            {% include "_admin_acl_rules_publish_buttons.tpl" %}
        {% endif %}

        <ul class="nav nav-tabs">
            <li class="{% if kind == `rsc` %}active{% endif %}">
                <a href="{% url admin_acl_rules_rsc %}">{_ Content _}</a>
            </li>
            <li class="{% if kind == `collab` %}active{% endif %}">
                <a href="{% url admin_acl_rules_collab %}">{_ Collaboration groups _}</a>
            </li>
            <li class="{% if kind == `module` %}active{% endif %}">
                <a href="{% url admin_acl_rules_module %}">{_ Modules _}</a>
            </li>
            <li class="{% if kind == `upload` %}active{% endif %}">
                <a href="{% url admin_acl_rules_upload %}">{_ File uploads _}</a>
            </li>
            <li class="{% if kind == `options` %}active{% endif %}">
                <a href="{% url admin_acl_rules_options %}">{_ Options _}</a>
            </li>
        </ul>

        <div class="widget">
            {% block filter %}
            {% endblock %}

            <div class="acl">
                {% block content_acl %}
                {% endblock %}
            </div>
        </div>

        {# these are needed for translation string generation #}
        <!-- {_ view (acl action) _} {_ insert (acl action) _} {_ edit (acl action) _} {_ delete (acl action) _} {_ link (acl action) _} -->
    {% endwith %}
{% else %}
    <p class="alert alert-danger">
        <strong>{_ Not allowed. _}</strong>
        {_ You are not allowed to view this page. _}
    </p>
{% endif %}
{% endblock %}
