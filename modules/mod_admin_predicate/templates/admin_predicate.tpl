{% extends "admin_base.tpl" %}

{% block title %} Predicates {% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
<div class="edit-header">
    
    <h2>{_ Predicates _}</h2>

    <p>{_ A predicate denotes traits or aspects of a page and expresses a relationship between two pages.
    The relation is always directed, from the subject to the object.<br/>Predicates are defined in ontologies like <a href="http://sioc-project.org/">SIOC</a>.  On this page you can define the predicates known to Zotonic. _}</p>
    
    {% if editable %}
    <div class="well">
        {% button class="btn btn-primary" text=_"Make a new predicate" action={dialog_predicate_new title=""} %}
    </div>
    {% endif %}

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ Title _}</th>
                <th width="20%">{_ Name _}</th>
                <th width="40%">{_ URI _}</th>
                <th width="20%">&nbsp;</th>
            </tr>
        </thead>

        <tbody>
            {% for name,p in m.predicate %}
            <tr id="{{ #li.name }}" data-href="{% url admin_edit_rsc id=p.id %}">
                <td>{{ p.title|default:"&nbsp;" }}</td>
                <td>{{ p.name|default:"&nbsp;" }}</td>
                <td>{{ p.uri|default:"&nbsp;" }}</td>
                <td>
                    <div class="pull-right">
                        {% button class="btn btn-mini" disabled=p.is_protected text="delete" action={dialog_predicate_delete id=p.id on_success={slide_fade_out target=#li.name}} %}
                        <a href="{% url admin_edit_rsc id=p.id %}" class="btn btn-mini">{_ edit _}</a>
                    </div>                        
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
            </li>
            {% empty %}
            <li>
                No predicates found.
            </li>
            {% endfor %}
        </ul>

    </div>
</div>
{% endwith %}
{% endblock %}
