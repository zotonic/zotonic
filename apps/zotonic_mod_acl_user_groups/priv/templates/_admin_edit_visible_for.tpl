
<div class="form-group row">
    <label class="control-label col-md-3">{_ Content group _}</label>
    <div class="col-md-9">
		<select class="form-control" id="{{ #content_group_id }}" name="content_group_id">
		{% for cg in m.hierarchy.content_group.tree_flat %}
			<option value="{{ cg.id }}"
				{% if cg.id == id.content_group_id or (not id.content_group_id and cg.id.name == 'default_content_group') %}
					selected
				{% elseif id and cg.id /= id.content_group_id and not m.acl_rule.can_move[cg.id][id] %}
					disabled
				{% endif %}
			>
				{{ cg.indent }}{{ cg.id.title }}
			</option>
		{% endfor %}
		</select>
    </div>
</div>
