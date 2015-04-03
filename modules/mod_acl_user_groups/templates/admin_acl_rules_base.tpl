{% extends "admin_base.tpl" %}

{% block title %}{_ ACL rules _}{% endblock %}

{% block content %}
    <style>
        .acl-rule-row {
        padding: 5px 0;
        }
        .acl-well {
        margin-top: 40px;
        }
        div.acl .checkbox-inline {
        margin-top: 5px;
        }
    </style>
    
    <div class="admin-header">
        <h2>{_ Access control rules _}</h2>
        <ul class="nav nav-tabs">
            <li class="{% if kind == `rsc` %}active{% endif %}"><a href="{% url admin_acl_rules_rsc %}">{_ Content _}</a></li>
            <li class="{% if kind == `module` %}active{% endif %}"><a href="{% url admin_acl_rules_module %}">{_ Modules _}</a></li>
            <li class="{% if kind == `upload` %}active{% endif %}"><a href="{% url admin_acl_rules_upload %}">{_ File Uploads _}</a></li>
            <li class="{% if kind == `test` %}active{% endif %}"><a href="{% url admin_acl_rules_test %}">{_ Test ACL _}</a></li>
        </ul>
    </div>

    <div class="acl">
        {% block content_acl %}
        {% endblock %}
    </div>

    {# these are needed for translation string generation #}
    <!-- {_ view (acl action) _} {_ insert (acl action) _} {_ edit (acl action) _} {_ delete (acl action) _} {_ link (acl action) _} -->
    
{% endblock %}
    
