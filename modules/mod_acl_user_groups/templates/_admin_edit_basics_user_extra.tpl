<div class="col-md-6">
    <div class="well">
        <h4 style="margin-top: 0">{_ User groups _}</h4>

        {% for cg in m.hierarchy.acl_user_group.tree_flat %}
            <div class="checkbox">
		        <label>{{ cg.indent }} <input name="hasusergroup" value="{{ cg.id }}" type="checkbox" {% if cg.id|member:id.o.hasusergroup %}checked="checked"{% endif %} /> {{ cg.id.title }}</label>
            </div>
        {% endfor %}

    </div>
</div>
