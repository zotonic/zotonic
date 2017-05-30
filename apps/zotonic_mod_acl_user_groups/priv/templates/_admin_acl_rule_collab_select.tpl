{% if content_group_id.is_a.acl_collaboration_group %}
    <p class="form-control-static">
        <span class="z-icon z-icon-user"></span> {{ content_group_id.title }}
        <input type="hidden" name="content_group_id" value="{{ content_group_id }}" />
        <a href="#" id="{{ #collab_group_id_rm }}" class="btn">&times;</a>
        {% wire id=#collab_group_id_rm
                action={update target="acl-cg-collab-select"
                            template="_admin_acl_rule_collab_select.tpl"
                            content_group_id=`undefined`}
        %}
    </p>
{% else %}
    {% with m.hierarchy.content_group.tree_flat as cg_tree_flat %}
    {% with m.rsc.system_content_group.id as system_id %}
        <select class="form-control" id="{{ #content_group_id }}" name="content_group_id">
            <optgroup label="{_ Content groups _}">
                <option value="">{_ All normal content groups _}</option>
                {% for cg in cg_tree_flat %}
                    {% if cg.id /= system_id and not system_id|member:cg.path %}
                        <option value="{{ cg.id }}" {% if cg.id == content_group_id %}selected{% endif %}>
                            {{ cg.indent }} {{ cg.id.title }}
                        </option>
                    {% endif %}
                {% endfor %}
            </optgroup>
            {% if m.rsc.acl_collaboration_group.id as cgid %}
                <optgroup label="{{ cgid.title }}">
                    <option value="{{ cgid }}" {% if cgid == content_group_id %}selected{% endif %}>
                        {_ All collaboration groups _}
                    </option>
                </optgroup>
            {% endif %}
            <optgroup label="{_ System content groups _}">
                {% for cg in cg_tree_flat %}
                    {% if cg.id == system_id or system_id|member:cg.path%}
                        <option value="{{ cg.id }}" {% if cg.id == content_group_id %}selected{% endif %}>
                            {{ cg.indent }} {{ cg.id.title }}
                        </option>
                    {% endif %}
                {% endfor %}
            </optgroup>
        </select>
    {% endwith %}
    {% endwith %}
{% endif %}

{#
<input id="{{ #collab_group_id }}" type="hidden" name="collab_group_id" value="{{ id }}" />

<p>
    <span class="z-icon z-icon-user"></span> {{ id.title }}
    <a href="#" id="{{ #collab_group_id_rm }}" class="btn">&times;</a>
</p>

{% javascript %}
    $('#{{ #collab_group_id }}')
        .closest('form')
        .find('select[name=content_group_id]')
        .closest('div')
        .hide();

    $('#{{ #collab_group_id_rm }}').click(function() {
        $('#{{ #collab_group_id }}')
            .closest('form')
            .find('select[name=content_group_id]')
            .closest('div')
            .show();
        $('#{{ #collab_group_id }}')
            .closest('div')
            .html('');
    });
{% endjavascript %}
#}
