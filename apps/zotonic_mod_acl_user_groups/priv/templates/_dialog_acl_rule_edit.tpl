{% with m.acl_rule[kind][id] as rule %}
    {% if rule %}
        {% wire id=#ruleform type="submit"
                action={dialog_close}
                postback={update_rule id=rule.id kind=kind}
                delegate=`admin_acl_rules`
        %}
    {% else %}
        {% wire id=#ruleform type="submit"
                action={dialog_close}
                postback={add_rule kind=kind}
                delegate=`admin_acl_rules`
        %}
    {% endif %}

    {% if rule.managed_by %}
        <p>
            {_ This rule is managed by module _}
            <strong>{{ m.modules.info[rule.managed_by].title|default:rule.managed_by }}</strong>
        </p>
    {% endif %}

    <form id="{{ #ruleform }}" method="POST" action="postback" class="form form-horizontal">
        <fieldset {% if rule.managed_by %}disabled="disabled"{% endif %}>

        <div class="form-group">
            <label class="col-sm-3">
            </label>
            <div class="col-sm-7 checkbox">
                <label>
                    <input type="checkbox" id="{{ #is_block }}" name="is_block" value="on"
                        {% if rule.is_block %}checked="checked"{% endif %} />
                    {_ Deny _}
                </label>
            </div>
        </div>

        {% if kind == `rsc` or kind == `module` %}
        <div class="form-group">
            <label class="col-sm-3">
                {_ ACL user group _}
            </label>
            <div class="col-sm-7">
                <select class="form-control" id="{{ #user_group_id }}" name="acl_user_group_id">
                    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                        <option value="{{ cg.id }}" {% if cg.id == rule.acl_user_group_id %}selected{% endif %}>
                            {{ cg.indent }} {{ cg.id.title }}
                        </option>
                    {% endfor %}
                </select>
            </div>
        </div>
        {% endif %}

        {% if kind == `rsc` %}
            <div class="form-group">
                <label class="col-sm-3">
                    {_ Content group _}
                </label>
                <div class="col-sm-7">
                    <div id="acl-cg-collab-select">
                        {% include "_admin_acl_rule_collab_select.tpl"
                                    content_group_id=rule.content_group_id
                        %}
                    </div>

                    <div style="padding: 10px 0">
                        <input id="dialog-collab-search" name="dialog-collab-search"
                               class="form-control nosubmit" type="text"
                               placeholder="{_ Find collaboration group _}" value="" />
                    </div>
                    <ul id="dialog-collab-found" class="do_feedback list-unstyled"
                        data-feedback='{ "trigger": "dialog-collab-search", "delegate": "admin_acl_rules" }'>
                    </ul>
                    {% javascript %}
                    {% endjavascript %}
                </div>
            </div>

            <div>
            </div>
        {% endif %}

        {% if kind == `rsc` or kind == `collab` %}
            <div class="form-group">
                <label class="col-sm-3">
                    {_ Category _}
                </label>
                <div class="col-sm-7">
                    <select class="form-control" id="{{ #category_id }}" name="category_id">
                        <option value="">{_ All _}</option>
                        {% for cg in m.hierarchy[`$category`].tree_flat %}
                            <option value="{{ cg.id }}" {% if cg.id == rule.category_id %}selected{% endif %}>
                                {{ cg.indent }} {{ cg.id.title }}
                            </option>
                        {% endfor %}
                    </select>
                </div>
            </div>
            <div class="form-group">
                <label class="col-sm-3">
                </label>
                <div class="col-sm-7 checkbox">
                    <label>
                        <input type="checkbox" name="is_category_exact" value="1" {% if rule.is_category_exact %}checked{% endif %}>
                        {_ Exactly this category, no sub-categories _}
                    </label>
                </div>
            </div>
        {% endif %}

        {% if kind == `module` %}
            <div class="form-group">
                <label class="col-sm-3">
                    {_ Module _}
                </label>
                <div class="col-sm-7">
                    <select class="form-control" id="{{ #module }}" name="module">
                        <option value="">{_ All _}</option>
                        {% for mod, name in m.modules.all %}
                            <option value="{{ mod }}" {% if mod == rule.module %}selected{% endif %}>{{ mod }} ({{name }})</option>
                        {% endfor %}
                    </select>
                </div>
            </div>
        {% endif %}


        {% if kind == `rsc` or kind == `collab` %}
            <div class="form-group">
                <label class="col-sm-3">
                    {_ Own content _}
                </label>
                <div class="col-sm-7 checkbox">
                    <label>
                        <input type="checkbox" id="{{ #is_owner }}" name="is_owner" value="on"
                            {% if rule.is_owner %}checked="checked"{% endif %} /> {_ Manage own _}
                    </label>
                </div>
            </div>
        {% endif %}

        <div class="form-group">
            <label class="col-sm-3">
                {_ Permissions _}
            </label>
            <div class="col-sm-7 checkbox">
                {% for action, label in m.acl_rule[kind].actions %}
                    <label>
                        <input type="checkbox" name="action${{ action }}" value="on"
                            {% if rule.actions[action] %}checked="checked"{% endif %} /> {{ label }}
                    </label><br/>
                {% endfor %}
            </div>
        </div>

        </fieldset>

        <div class="modal-footer clearfix">
            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
            {% button type="submit" class="btn btn-primary" text=_"Save" %}

            {% if rule %}
                {% button class="btn btn-danger pull-left"
                            action={dialog_close}
                            postback={remove_rule id=rule.id kind=kind}
                            delegate=`admin_acl_rules`
                            text=_"Delete" tag="a"
                %}
            {% endif %}
        </div>
    </form>
{% endwith %}
