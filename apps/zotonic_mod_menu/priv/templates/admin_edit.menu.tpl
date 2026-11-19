{% extends "admin_edit.tpl" %}

{% block admin_edit_form_top %}
{# Edit the menu #}
<div class="row">
    <div id="menu-editor" class="col-lg-12 col-md-12">
        {% include "_admin_menu_menu_view.tpl" id=id %}
    </div>
</div>
{% if m.acl.use.mod_menu %}
<details class="widget"><summary class="widget-header">{_ Menu maintenance _}</summary>
<div class="widget-content">{% include "_admin_menu_maintenance.tpl" %}</div></details>
{% endif %}
{% endblock %}
