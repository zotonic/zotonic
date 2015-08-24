{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing access control to rsc  #}

{% block widget_title %}
{_ Access control _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Access control"|escapejs }}', text: '{{ _"Define who can see or edit this page."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-acl{% endblock %}

{% block widget_content %}
<fieldset>
    {% include "_admin_edit_visible_for.tpl" id=id %}
</fieldset>
{% endblock %}
