{% extends "admin_edit.tpl" %}

{% block admin_edit_form_top %}
{# Edit the menu #}
<div class="row admin-header">
    <div id="menu-editor" class="col-lg-12 col-md-12">
        {% include "_admin_menu_menu_view.tpl" id=id %}
    </div>
</div>
{% endblock %}
