{# Show an object with an unlink option. Used in the admin_edit #}
{% with m.rsc[object_id].title as title %}
	{% sortable id=#unlink_wrapper tag=edge_id %}
	<li id="{{ #unlink_wrapper }}" class="rsc-edge do_unlink">
		<span class="clearfix">
			<span class="unlink-mover"></span>
			<span id="{{ #unlink }}" class="unlink-cross"></span>
			<span class="unlink-item"><a href="{% url admin_edit_rsc id=object_id %}">{{ title }}</a></span>
		</span>
	</li>
{% endwith %}

{% wire id=#unlink action={unlink subject_id=subject_id edge_id=edge_id hide=#unlink_wrapper} %}
