{% if is_editable %}
    <div class="col-md-4">
        <select class="form-control" id="{{ #user_group_id }}" name="acl_user_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
            {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                <option value="{{ cg.id }}" {% if cg.id == rule.acl_user_group_id %}selected{% endif %}>
                    {{ cg.indent }} {{ cg.id.title }}
                </option>
            {% endfor %}
        </select>
    </div>
    <div class="col-md-4">
        {% if not is_new and rule.content_group_id.is_a.acl_collaboration_group %}
        <p class="form-control-static">
            <span class="z-icon z-icon-user"></span> {{ rule.content_group_id.title }}
            <input type="hidden" name="content_group_id" value="{{ rule.content_group_id }}" />
        </p>
        {% else %}
            {% if is_new %}
                <div id="acl-cg-select">
            {% endif %}

            {% with m.hierarchy.content_group.tree_flat as cg_tree_flat %}
            {% with m.rsc.system_content_group.id as system_id %}
                <select class="form-control" id="{{ #content_group_id }}" name="content_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
                    <optgroup label="{_ Content groups _}">
                        <option value="">{_ All normal content groups _}</option>
                        {% for cg in cg_tree_flat %}
                            {% if cg.id /= system_id and not system_id|member:cg.path %}
                                <option value="{{ cg.id }}" {% if cg.id == rule.content_group_id %}selected{% endif %}>
                                    {{ cg.indent }} {{ cg.id.title }}
                                </option>
                            {% endif %}
                        {% endfor %}
                    </optgroup>
                    {% if m.rsc.acl_collaboration_group.id as cgid %}
                        <optgroup label="{{ cgid.title }}">
                            <option value="{{ cgid }}" {% if cgid == rule.content_group_id %}selected{% endif %}>
                                {_ All collaboration groups _}
                            </option>
                        </optgroup>
                    {% endif %}
                    <optgroup label="{_ System content groups _}">
                        {% for cg in cg_tree_flat %}
                            {% if cg.id == system_id or system_id|member:cg.path%}
                                <option value="{{ cg.id }}" {% if cg.id == rule.content_group_id %}selected{% endif %}>
                                    {{ cg.indent }} {{ cg.id.title }}
                                </option>
                            {% endif %}
                        {% endfor %}
                    </optgroup>
                </select>
            {% endwith %}
            {% endwith %}

            {% if is_new %}
                    <br/>
                    <a href="#" class="btn btn-default" id="{{ #collab_select }}">{{ m.rsc.acl_collaboration_group.title }}</a>
                </div>
                <div id="acl-cg-collab-select">
                </div>

                {% wire id=#collab_select
                        action={dialog_open
                                template="_action_dialog_connect.tpl" 
                                title=[_"Select a: ", m.rsc.acl_collaboration_group.title] 
                                category=`acl_collaboration_group`
                                tabs_enabled=["find"]
                                delegate=`admin_acl_rules`
                            }
                %}

            {% endif %}
        {% endif %}
    </div>
    <div class="col-md-4">
        <select class="form-control" id="{{ #category_id }}" name="category_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
            <option value="">{_ All _}</option>
            {% for cg in m.hierarchy[`$category`].tree_flat %}
                <option value="{{ cg.id }}" {% if cg.id == rule.category_id %}selected{% endif %}>
                    {{ cg.indent }} {{ cg.id.title }}
                </option>
            {% endfor %}
        </select>
    </div>
{% else %}
    <div class="col-md-2">
        {% if rule.acl_user_group_id %}
            {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                {% if cg.id == rule.acl_user_group_id %}
                    {{ cg.indent }} {{ cg.id.title }}
                {% endif %}
            {% endfor %}
        {% else %}
            {_ All User Groups _}
        {% endif %}
    </div>
    <div class="col-md-2">
        {% if rule.content_group_id.is_a.acl_collaboration_group %}
            <span class="z-icon z-icon-user"></span> {{ rule.content_group_id.title }}
        {% elseif rule.content_group_id %}
            {% for cg in m.hierarchy.content_group.tree_flat %}
                {% if cg.id == rule.content_group_id %}
                    {{ cg.indent }} {{ cg.id.title }}
                {% endif %}
            {% endfor %}
        {% else %}
            {_ All Content Groups _}
        {% endif %}
    </div>

    <div class="col-md-2">
        {% if rule.category_id %}
            {{ rule.category_id.title }}
        {% else %}
            {_ All Categories _}
        {% endif %}
    </div>
{% endif %}
