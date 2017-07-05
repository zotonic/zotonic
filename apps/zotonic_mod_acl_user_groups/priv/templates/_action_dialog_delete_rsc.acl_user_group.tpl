<p>{_ Are you sure you want to delete the following user groups: _}</p>

<ul>
	{% for cg in m.hierarchy.acl_user_group[id].tree_flat %}
		<li>{{ cg.id.title }}</li>
	{% endfor %}
</ul>

{% if m.acl_user_group.is_used[id] %}
	{% wire id=#menu_delete type="submit"
			postback={delete_move id=id}
			delegate=`mod_acl_user_groups`
	%}
	<form id="{{ #menu_delete }}" action="postback" type="post">
		<p>{_ Move all users within these user groups to: _}</p>

		<div class="form-group">
			<select class="form-control" name="acl_user_group_id" id="{{ #cgid }}">
				<option></option>
				{% for cg in m.hierarchy.acl_user_group.tree_flat %}
					{% if cg.id /= id and not id|member:cg.path %}
						<option value="{{ cg.id }}">
							{{ cg.indent }} {{ cg.id.title }}
						</option>
					{% endif %}
				{% endfor %}
			</select>
			{% validate id=#cgid name="acl_user_group_id" type={presence} %}
		</div>

		<p>{_ Or click <strong>Delete all user groups</strong> to disconnect all users and delete the user groups. _}</p>

		<p>{_ This can not be undone. _}</p>

		<div class="modal-footer">
			{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
			{% button type="submit" class="btn btn-primary" text=_"Delete user group and move users" %}
			{% button tag="a"
					  class="btn btn-danger pull-left"
					  text=_"Delete all user groups"
					  postback={delete_all id=id}
					  delegate=`mod_acl_user_groups`
			%}
		</div>
	</form>
{% else %}
	<p>{_ There are no users within these user groups. _}</p>

	<div class="modal-footer">
		{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
		{% button class="btn btn-primary" text=_"Delete"
				postback={delete_all id=id if_empty}
				delegate=`mod_acl_user_groups`
		%}
	</div>
{% endif %}
