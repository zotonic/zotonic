{% extends "admin_acl_rules_base.tpl" %}

{% block active1 %}active{% endblock %}

{% block content_acl %}
    <div class="acl-well">
        <div id="acl-rules">
            {% live template="_acl_rules_list.tpl" topic="~site/acl-rules/edit-rebuild" kind=kind group=q.g is_editable=is_editable %}
        </div>
    </div>

    {% if is_editable %}
        <div class="acl-well">
            <h4>{_ New rule _}</h4>
            {% include "_admin_acl_rule_header.tpl" %}
            {% include "_admin_acl_rule_row.tpl" is_new %}
        </div>
    {% endif %}
{% endblock %}

{% block filter %}
<div class="clearfix acl-filter">
    <div class="pull-right">
        <div class="btn-group">
            <div class="btn-group">
                <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown">
                    {_ Show _}
                    {% if q.g %}
                        {{ m.rsc[q.g].title }}
                    {% else %}
                        {_ All _}
                    {% endif %}
                    <span class="caret"></span>
                </button>

                <input type="hidden" name="qgroup" id="cskrft-select" value="" />
                <ul class="dropdown-menu dropdown-menu-right" role="menu">
                    <li role="presentation" class="dropdown-header">
                        {_ Show this ACL user group first: _}
                    </li>
                    
		            {% for cg in m.hierarchy.acl_user_group.tree_flat %}
			            <li id="{{ cg.id }}" {% if cg.id == q.g %}class="active"{% endif %}>
				            <a href="?g={{ cg.id }}">
                                {{ cg.indent }} {{ cg.id.title }}
                            </a>
                        </li>
		            {% endfor %}

                    <li class="divider"></li>
                    <li class="">
                        <a href="?g=" class="cskrft-option" data-value="353">
                            <i class="glyphicon glyphicon-remove"></i> {_ Reset _}
                        </a>
                    </li>
                    
                </ul>
            </div>
        </div>
    </div>
</div>
{% endblock %}
