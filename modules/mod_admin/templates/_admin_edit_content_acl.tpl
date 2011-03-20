{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing access control to rsc  #}

{% block widget_title %}{_ Access control _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}


{% block widget_content %}
<div class="admin-form clearfix">
	<div class="notification notice">
		{_ Define who can see or edit this page. _}
	</div>

	{% include "_admin_edit_visible_for.tpl" id=id %}
</div>
{% endblock %}
