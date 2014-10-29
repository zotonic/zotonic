<span id="{{ #leaderimage }}">
	{% with id.depiction as depict %}
	{% if depict %}
		{% image depict mediaclass="admin-leader-image" class="admin-leader pull-left" title=depict.id.title %}
		{% wire id=" .leader" target=undefined action={dialog_edit_basics id=depict.id} %}
	{% endif %}
	{% endwith %}
</span>
