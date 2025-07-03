<div class="form-group">
    <label class="control-label">{_ Content group _}</label>
	<select class="form-control" id="{{ #content_group_id }}" name="content_group_id">
		{% if id.content_group_id.is_a.acl_collaboration_group %}
			<option value="{{ id.content_group_id }}" selected>
				{{ id.content_group_id.title }}
			</option>
			<option disabled></option>
		{% endif %}

		{% for cg in m.hierarchy.content_group.tree_flat %}
			<option value="{{ cg.id }}"
				{% if cg.id == id.content_group_id
					or (not id.content_group_id and cg.id.name == 'default_content_group')
				%}
					selected
				{% elseif id and cg.id /= id.content_group_id and not m.acl_rule.can_move[cg.id][id] %}
					disabled
				{% endif %}
			>
				{{ cg.indent }}{{ cg.id.title }}
			</option>
		{% endfor %}
	</select>

	{% if m.search::%{ cat: 'acl_collaboration_group', pagelen: 1 } %}
	    <br/>
	    <button type="button" class="btn btn-default" id="{{ #collab_select }}">
	        {% trans "Move to {title}…" title=m.rsc.acl_collaboration_group.title %}
	    </button>


	    {% wire id=#collab_select
	            action={dialog_open
	                    intent="select"
	                    template="_action_dialog_connect.tpl"
	                    subject_id=id
	                    title=[_"Select", " ", m.rsc.acl_collaboration_group.title]
	                    category=`acl_collaboration_group`
	                    tabs_enabled=["find"]
	                    delegate=`admin_acl_rules`
	                    nocatselect
	                    autoclose
	                }
	    %}
	{% endif %}
</div>

