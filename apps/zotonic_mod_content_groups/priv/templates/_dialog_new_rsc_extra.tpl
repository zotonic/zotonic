{% if not m.modules.active.mod_acl_user_groups %}
	<div class="form-group row">
		<label class="control-label col-md-3" for="{{ #category }}">{{ m.rsc.content_group.title }}</label>
		<div class="col-md-9">
			<select class="form-control" id="{{ #content_group_id }}" name="content_group_id">
				<option value=""></option>
				{% for cg in m.hierarchy.content_group.tree_flat %}
					<option value="{{ cg.id }}" {% if cg.id == id.content_group_id %}selected{% endif %}>
						{{ cg.indent }} {{ cg.id.title }}
					</option>
				{% endfor %}
			</select>
		</div>
	</div>
{% endif %}
