{% overrules %}

{% block category %}
    {% include "_admin_catcg.tpl" cat_id=m.rsc.person.id form=form %}
{% endblock %}

{% block user_account %}
    <h4>{_ User groups _}</h4>

    <div class="form-group row">
        <div class="col-md-3"></div>
        <div class="col-md-9">
            {% with id.o.hasusergroup|default:(m.identity[id].is_user|if:[m.rsc.acl_user_group_members.id]:[]) as ugs %}
                {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                    <div class="checkbox">
                        <label>{{ cg.indent }} <input name="hasusergroup_list[]" value="{{ cg.id }}" type="checkbox" {% if cg.id|member:ugs %}checked="checked"{% endif %} /> {{ cg.id.title }}</label>
                    </div>
                {% endfor %}
            {% endwith %}
        </div>
    </div>
{% endblock %}
