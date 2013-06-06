{% extends "admin_edit.tpl" %}

{% block admin_edit_form_top %}
{# Edit the menu #}
<div class="row-fluid edit-header">
    <div id="menu-editor" class="span12">
        {% include "_admin_menu_menu_view.tpl" id=id %}
    </div>
</div>
{% endblock %}
