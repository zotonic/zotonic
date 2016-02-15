{% if is_editable %}
    {% if rule %}
        {% wire id=#ruleform type="submit"
                action={mask target=#ruleform}
                postback={update_rule id=rule.id kind=kind}
                delegate=`admin_acl_rules`
        %}
    {% else %}
        {% wire id=#ruleform type="submit"
                postback={add_rule kind=kind}
                delegate=`admin_acl_rules`
        %}
    {% endif %}
    <form id="{{ #ruleform }}" class="row acl-rule-row acl-rule-{{ rule.id }} {% if rule.is_block %}is_block{% endif %}" method="post" action="postback">

        {% if rule.managed_by %}
            <fieldset disabled="disabled">
        {% endif %}

        <div class="col-md-1">
            <label class="checkbox-inline">
                <input type="checkbox" id="{{ #is_block }}" name="is_block" value="on"
                    {% if rule.is_block %}checked="checked"{% endif %}
                    {% if rule %}onchange="$(this.form).submit()"{% endif %} />
                    {% if rule.is_block %}<span class="z-icon z-icon-minus-circle"></span> {% endif %}{_ deny _}
            </label>
        </div>

        <div class="col-md-6">
            <div class="row">
                <div class="col-md-4">
                    <select class="form-control" id="{{ #user_group_id }}" name="acl_user_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
                        {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                            <option value="{{ cg.id }}" {% if cg.id == rule.acl_user_group_id %}selected{% endif %}>
                                {{ cg.indent }} {{ cg.id.title }}
                            </option>
                        {% endfor %}
                    </select>
                </div>

                {% if kind == "rsc" %}
                    <div class="col-md-4">
                        <select class="form-control" id="{{ #content_group_id }}" name="content_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
                            <option value="">{_ All _}</option>
                            {% for cg in m.hierarchy.content_group.tree_flat %}
                                <option value="{{ cg.id }}" {% if cg.id == rule.content_group_id %}selected{% endif %}>
                                    {{ cg.indent }} {{ cg.id.title }}
                                </option>
                            {% endfor %}
                        </select>
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
                {% elseif kind == "module" %}
                    <div class="col-md-8">
                        <select class="form-control" id="{{ #module }}" name="module" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
                            <option value="">{_ All _}</option>
                            {% for mod, name in m.modules.all %}
                                <option value="{{ mod }}" {% if mod == rule.module %}selected{% endif %}>{{ mod }} ({{name }})</option>
                            {% endfor %}
                        </select>
                    </div>
                {% endif %}
            </div>
            {% if rule.managed_by %}
                <div class="row">
                    <div class="col-md-12">
                        {_ This rule is managed by module _}
                        <strong>{{ m.modules.info[rule.managed_by].title|default:rule.managed_by }}</strong>
                    </div>
                </div>
            {% endif %}
        </div>

        <div class="col-md-5">

            {% if is_new %}
                {% button
                    text=_"Add"
                    class="btn btn-primary pull-right"
                    icon="z-icon z-icon-plus"
                    type="submit"
                %}
            {% else %}
                {% button
                    text=_"Remove"
                    class="btn btn-danger pull-right"
                    icon="z-icon z-icon-minus"
                    postback={remove_rule id=rule.id kind=kind} delegate=`admin_acl_rules`
                %}
            {% endif %}

            <div>
                {% for action, label in m.acl_rule[kind].actions %}
                    <label class="checkbox-inline">
                        <input type="checkbox" name="action${{ action }}" value="on"
                            {% if rule.actions[action] %}checked="checked"{% endif %}
                            {% if rule %}onchange="$(this.form).submit()"{% endif %} /> {{ label }}
                    </label>
                {% endfor %}

                {% if kind == "rsc" %}
                    <label class="checkbox-inline">
                        <input type="checkbox" id="{{ #is_owner }}" name="is_owner" value="on"
                            {% if rule.is_owner %}checked="checked"{% endif %}
                            {% if rule %}onchange="$(this.form).submit()"{% endif %} /> {_ manage own _}
                    </label>
                {% endif %}
            </div>
        </div>

        {% if rule.managed_by %}
            </fieldset>
        {% endif %}
    </form>
{% else %}
    <div class="row acl-rule-row acl-rule-{{ rule.id }} {% if rule.is_block %}bg-danger{% endif %}">
        <div class="col-md-1">
             {% if rule.is_block %}
                <span class="z-icon z-icon-minus-circle"></span>
            {% endif %}
        </div>

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

        {% if kind == "rsc" %}
            <div class="col-md-2">
               {% if rule.content_group_id %}
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
        {% elseif kind == "module" %}
            <div class="col-md-4">
                {% if rule.module %}
                    <span class="text-muted">{{ rule.module }}</span> {{ m.modules.info[rule.module].title }}
                {% else %}
                    {_ All Modules _}
                {% endif %}
            </div>
        {% endif %}

        <div class="col-md-5">
            <div>
                {% if kind == "rsc" %}
                    {% if rule.is_owner%}
                        <span class="label label-primary">{_ manage own _}</span>
                    {% else %}
                        <span class="label">{_ manage own _}</span>
                    {% endif %}
                {% endif %}

                {% for action, label in m.acl_rule[kind].actions %}
                    {% if rule.actions[action] %}
                        <span class="label label-default">{{ label }}</span>
                    {% else %}
                        <span class="label">{{ label }}</span>
                    {% endif %}
                {% endfor %}
            </div>
        </div>
    </div>
{% endif %}
