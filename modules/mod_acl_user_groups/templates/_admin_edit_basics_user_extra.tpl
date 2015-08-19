<div class="col-md-6">
    <div class="well">
        <h4 style="margin-top: 0">{_ User groups _}</h4>

        {% with id.o.hasusergroup|default:(m.identity[id].is_user|if:[m.rsc.acl_user_group_members.id]:[]) as ugs %}
	        {% for cg in m.hierarchy.acl_user_group.tree_flat %}
	            <div class="checkbox">
			        <label>{{ cg.indent }} <input name="hasusergroup" value="{{ cg.id }}" type="checkbox" {% if cg.id|member:ugs %}checked="checked"{% endif %} /> {{ cg.id.title }}</label>
	            </div>
	        {% endfor %}
	    {% endwith %}

    </div>
</div>
