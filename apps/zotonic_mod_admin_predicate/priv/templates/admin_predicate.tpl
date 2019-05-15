{% extends "admin_base.tpl" %}

{% block title %}{_ Predicates _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
<div class="admin-header">

    <h2>{_ Predicates _}</h2>

    <p>{_ A predicate denotes traits or aspects of a page and expresses a relationship between two pages.
    The relation is always directed, from the subject to the object.<br/>Predicates are defined in ontologies like <a href="http://sioc-project.org/">SIOC</a>.  On this page you can define the predicates known to Zotonic. _}</p>
</div>

{% if m.acl.insert.predicate %}
<div class="well">
    {% button class="btn btn-primary" text=_"Make a new predicate" action={dialog_predicate_new title=""} %}
</div>
{% endif %}

<div>
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ Title _}</th>
                <th width="20%">{_ Name _}</th>
                <th width="40%">{_ URI _}</th>
                <th width="20%">&nbsp;</th>
            </tr>
        </thead>

        <tbody id="predicate-list">
            {% for name,p in m.predicate %}
            <tr id="{{ #li.name }}" data-href="{% url admin_edit_rsc id=p.id %}" data-id="{{ p.id }}">
                <td>{{ p.title|default:"&nbsp;" }}</td>
                <td>{{ p.name|default:"&nbsp;" }}</td>
                <td>{{ p.uri|default:"&nbsp;" }}</td>
                <td>
                    <div class="pull-right buttons">
                        {% if m.acl.is_allowed.delete[p.id] %}
                            {% button class="btn btn-default btn-xs" disabled=p.is_protected text=_"delete" action={dialog_delete_rsc id=p.id} %}
                        {% endif %}
                        {% if m.acl.is_allowed.update[p.id] %}
                            <a href="{% url admin_edit_rsc id=p.id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                        {% endif %}
                    </div>
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
            </li>
            {% empty %}
            <li>
                {_ No predicates found. _}
            </li>
            {% endfor %}
        </ul>

    </div>
</div>
{% endwith %}

{% javascript %}
    cotonic.broker.subscribe("bridge/origin/model/rsc/event/+", function(msg) {
        if (msg.payload._type == 'rsc_update_done' && msg.payload.action == 'delete') {
            $('#predicate-list tr[data-id='+msg.payload.id+']').remove();
        }
    });
{% endjavascript %}

{% endblock %}


