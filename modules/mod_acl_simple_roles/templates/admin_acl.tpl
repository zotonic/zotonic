{% extends "admin_base.tpl" %}

{% block title %}{_ Admin Access Control _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}

<div class="edit-header">

    <h2>{_ Access Control - Roles Overview _}</h2>

    <p>{_ Access control controls what an user is allowed to do and see.  The roles define different groups of rights.  Users can be made member of multiple roles. _}</p>
    
    {% if editable %}
    <div class="well">
        {% button class="btn btn-primary" text=_"make a new acl role" action={dialog_new_rsc cat="acl_role" nocatselect} %}
    </div>
    {% endif %}
</div>
        
<div>
    <h3>{_ ACL role overview _}</h3>
    <hr />

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ Title _}</th>
                <th width="10%">{_ View all _}</th>
                <th width="70%">{_ Rights _}</th>
            </tr>
        </thead>

        <tbody>
            {% for title, id in m.search[{all_bytitle cat="acl_role"}] %}
            <tr id="{{ #tr.id }}" data-href="{% url admin_edit_rsc id=id %}">
                {% with m.rsc[id].acl as acl %}
                <td>{{ title|default:"&nbsp;" }}</td>
                <td>{{ acl.view_all|yesno:_"view all,&mdash;" }}</td>
                <td>
                    <div class="pull-right">
                        {% button class="btn btn-mini" disabled=p.is_protected text=_"delete" action={dialog_delete_rsc id=id on_success={slide_fade_out target=#tr.id}} %}
                        {% button class="btn btn-mini" text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
                    </div>
                    <strong>{_ Categories _}</strong>:
                    {% for cat in acl.categories %}
                    {{ cat }}{% if not forloop.last %}, {% endif %}
                    {% endfor %}<br/>
                    <strong>{_ Modules _}</strong>:
                    {% for mod in acl.modules %}
                    {{ mod }}{% if not forloop.last %}, {% endif %}
                    {% endfor %}
                </td>
                {% endwith %}
            </tr>
            {% empty %}
            <tr>
                <td colspan="3">
                    {_ No ACL roles found. _}
                </td>
            </tr>
            {% endfor %}
        </tbody>
    </table>
</div>
{% endwith %}
{% endblock %}
