{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing access control to rsc  #}

{% block widget_title %}
{_ Category &amp; Content group _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Access control"|escapejs }}', text: '{{ _"Define who can see or edit this page."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-catcg{% endblock %}

{% block widget_content %}
<fieldset>
    <p>
    	{_ This page is a _}
    	<strong>{{ id.category_id.title }}</strong>
    	{_ in the group _}
    	<strong>{{ id.content_group_id.title }}</strong>
    </p>

    {% if m.acl_rule.can_insert[id.content_group_id][id.category_id] %}
	    <a href="#" id="{{ #changecg }}" class="btn btn-default">{_ Change category and/or content group... _}</a>
		{% wire id=#changecg
				action={submit closest}
				action={dialog_open title=_"Category &amp; Content group" template="_action_dialog_change_category.tpl" id=id}
		%}
	{% endif %}
</fieldset>
{% endblock %}
