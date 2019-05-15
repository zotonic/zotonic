{% extends "admin_acl_rules_base.tpl" %}

{% block active1 %}active{% endblock %}

{% block content_acl %}
    <div class="acl-well">
        <div id="acl-rules">
            {% live template="_admin_acl_rules_list.tpl"
                    topic="model/acl_user_groups/event/acl-rules/edit-rebuild"
                    kind=kind
                    group=q.g
                    is_editable=is_editable
            %}
        </div>
    </div>

    {#
        {% if is_editable %}
            <div class="acl-well">
                <h4>{_ New rule _}</h4>
                {% include "_admin_acl_rules_list_header.tpl" %}
                {% include "_admin_acl_rule_row.tpl" is_new %}
            </div>
        {% endif %}
    #}
{% endblock %}

{% block filter %}
{% if kind == `collab` %}
<div class="acl">
    <div class="alert alert-info">
        {_ Collaboration groups are user groups where all content belongs to the group. _}
        {_ They are managed by their own manager and use the access rules below. _}<br/>
        {_ Collaboration groups are added by creating a new page in the category: _}
        <em>{{ m.rsc.acl_collaboration_group.title }}</em>
    </div>

    {% if is_editable %}
        {% wire id=#cgrights
            type="submit"
            postback=`acl_collab_config`
            delegate=`admin_acl_rules`
        %}
        <fieldset>
            <legend>{_ Edit &amp; link rights on the collaboration groups _}</legend>
            <form id="{{ #cgrights }}" action="postback">
                <div class="row">
                    <div class="col-md-4 col-sm-6">
                        <label>{_ Edit the collaboration group _}</label>
                        <select class="form-control" name="collab_group_update" id="{{ #cgedit }}">
                            <option value=""></option>
                            <option value="manager" {% if m.acl_user_group.collab_group_update == 'manager' %}selected{% endif %}>{_ Group managers can edit _}</option>
                            <option value="member" {% if m.acl_user_group.collab_group_update == 'member' %}selected{% endif %}>{_ Group members and managers can edit _}</option>
                        </select>
                    </div>
                    <div class="col-md-4 col-sm-6">
                        <label>{_ Add links to the collaboration group _}</label>
                        <select class="form-control" name="collab_group_link" id="{{ #cglink }}">
                            <option value=""></option>
                            <option value="manager" {% if m.acl_user_group.collab_group_link == 'manager' %}selected{% endif %}>{_ Group managers can link _}</option>
                            <option value="member" {% if m.acl_user_group.collab_group_link == 'member' %}selected{% endif %}>{_ Group members and managers can link _}</option>
                        </select>
                    </div>
                    <div class="col-md-2">
                        <label>&nbsp;</label><br/>
                        <button class="btn btn-primary" type="submit">{_ Save _}</button>
                    </div>
                </div>
            </form>
            <p>
                <br/>
            </p>
        </fieldset>
    {% endif %}


    <fieldset>
        <legend>{_ Content rules for collaboration group members and managers _}</legend>

{% else %}
    <div class="clearfix acl-filter">
        <div class="pull-right">
            <div class="btn-group">
                <div class="btn-group">
                    <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown">
                        {_ Show _}
                        {% if q.g %}
                            {{ m.rsc[q.g].title }}
                        {% else %}
                            {_ all _}
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
                                <i class="glyphicon glyphicon-remove"></i> {_ All _}
                            </a>
                        </li>

                    </ul>
                </div>
            </div>
        </div>
    </div>
{% endif %}
{% if kind == `collab` %}
    </fieldset>
</div>
{% endif %}
{% endblock %}
