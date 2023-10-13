{% extends "admin_acl_rules_base.tpl" %}

{% block content_acl %}
    <div class="admin-padding">
        <p>{_ Options for the ACL module. _}</p>

        <div class="form-group">
            <label>
                <input id="author_is_owner" name="author_is_owner" type="checkbox" value="1"
                       {% if m.acl_rule.author_is_owner %}checked{% endif %}
                       {% if not m.acl.use.mod_acl_user_groups %}disabled{% endif %}
                > {_ Consider resource authors to be owner. _}
            </label>
            {% wire id="author_is_owner"
                    postback={set_config key=`author_is_owner`}
                    delegate=`mod_acl_user_groups`
            %}
            <p class="help-block">
                {_ If checked then users connected with an <em>author</em> predicate are considered resource owners for the ACL rule evaluation. This ensures that ACL rules marked as <em>manage own</em> are applicable to authors. _}
            </p>
        </div>
    </div>
{% endblock %}
