{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing access control to rsc  #}

{% block widget_title %}{_ Access control _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{{ _"Access control"|escapejs }}', text: '{{ _"Define who can see or edit this page."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>

{% include "_admin_edit_visible_for.tpl" id=id %}

{% endblock %}
