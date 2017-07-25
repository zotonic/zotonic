{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Content Group _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-content-group{% endblock %}

{% block widget_content %}
	<p>{_ This page is part of the group: _}</p>

	<select class="form-control" id="{{ #content_group_id }}" name="content_group_id">
		<option value="">{_ None _}</option>
		{% for cg in m.hierarchy.content_group.tree_flat %}
			<option value="{{ cg.id }}" {% if cg.id == id.content_group_id %}selected{% endif %}>
				{{ cg.indent }} {{ cg.id.title }}
			</option>
		{% endfor %}
	</select>

{% endblock %}
