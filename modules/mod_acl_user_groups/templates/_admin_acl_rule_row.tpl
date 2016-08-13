{% if is_editable %}
    {% wire id=#tr
            action={dialog_open
                        template="_dialog_acl_rule_edit.tpl"
                        title=_"Edit Rule"
                        id=rule.id kind=kind}
    %}
{% endif %}
<tr id="{{ #tr }}" {% if rule.is_block %}class="is_block alert-danger"{% endif %}{% if is_editable %}style="cursor:pointer"{% endif %}>
    <td>
        {% if rule.is_block %}
            <span class="z-icon z-icon-minus-circle"></span>
        {% endif %}
    </td>

    {% if kind == `rsc` %}
        {% include "_admin_acl_rule_row_rsc.tpl" %}
    {% elseif kind == `module` %}
        {% include "_admin_acl_rule_row_module.tpl" %}
    {% elseif kind == `collab` %}
        {% include "_admin_acl_rule_row_collab.tpl" %}
    {% endif %}

    <td>
        {% if kind == `rsc` or kind == `collab` %}
            {% if rule.is_owner %}
                <span class="label label-{% if rule.is_block %}danger{% else %}primary{% endif %}">{_ manage own _}</span>
            {% else %}
                <span class="label label-default" style="opacity: 0.3">{_ manage own _}</span>
            {% endif %}
        {% endif %}

        {% for action, label in m.acl_rule[kind].actions %}
            {% if rule.actions[action] %}
                <span class="label label-{% if rule.is_block %}danger{% else %}primary{% endif %}">{{ label }}</span>
            {% else %}
                <span class="label label-default" style="opacity: 0.3">{{ label }}</span>
            {% endif %}
        {% endfor %}
    </td>
    <td>
        {% if is_editable %}
            <a class="btn btn-primary btn-xs" id="{{ #edit }}">{_ Edit Rule _}</a>
            {% wire id=#edit
                    action={dialog_open
                                template="_dialog_acl_rule_edit.tpl"
                                title=_"Edit Rule"
                                id=rule.id kind=kind}
            %}
        {% endif %}
    </td>
</tr>
