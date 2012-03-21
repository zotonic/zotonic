{% extends "admin_base.tpl" %}

{% block title %}{_ Users _}{% endblock %}

{% block content %}
<div class="edit-header">

    <h2>{_ Users _}</h2>
    
    <p>
        {_ Every page/person can be made into a user on the edit page.
        The difference between a user and a normal page is only
        that the former has logon credentials attached to its page record. _}
    </p>

    {% if m.acl.is_admin %}
    <div class="well">
        {% button class="btn btn-primary" text=_"Make a new user" action={dialog_user_add on_success={reload}} %}
    </div>
    {% else %}
    <div class="alert alert-info">{_ You need to be an administrator to add users. _}</div>
    {% endif %}
    
</div>

<div>    
    {% with m.acl.user as me %}

    {% with m.search.paged[{users text=q.qs page=q.page}] as result %}

    <h3 class="above-list ">
        {_ Users _}{% if q.qs %}, 
        {_ matching _} “{{ q.qs|escape }}”
        {% button text=_"show all" action={redirect dispatch="admin_user"} %}
        {% else %} {_ overview _}{% endif %}
    </h3>
    <hr />
    
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ Name _}</th>
                <th width="15%">{_ Username _}</th>
                <th width="10%">{_ Modified on _}</th>
                <th width="40%">{_ Created on _}</th>
            </tr>
        </thead>

        <tbody>
            {% for id, rank in result %}
            <tr id="{{ #tr.id }}" data-href="{% url admin_edit_rsc id=id %}">
                <td>{{ m.rsc[id].title|striptags }}</td>
                <td>{{ m.identity[id].username|escape }}{% if id == me %}  <strong>(that's you)</strong>{% endif %}</td>
                <td>{{ m.rsc[id].modified|date:"d M, H:i" }}</td>
                <td>
                    <div class="pull-right">
                        {% button class="btn btn-mini" action={dialog_set_username_password id=id} text=_"set username/ password" on_delete={slide_fade_out target=#tr.id} %}
                        {% if id /= 1 %}
                        {% button class="btn btn-mini" text=_"delete username" action={dialog_delete_username id=id on_success={slide_fade_out target=#tr.id}} %}
                        {% endif %}
                        {% button class="btn btn-mini" text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
                    </div>
                    {{ m.rsc[id].created|date:"d M, H:i" }}
                </td>
            </tr>

            {% empty %}
            <tr>
                <td colspan="4">
                    {_ No users found. _}
                </td>
            </tr>
            {% endfor %}
        </tbody>
    </table>

    {% pager result=result dispatch="admin_user" qargs %}

    {% endwith %}
    {% endwith %}

</div>
{% endblock %}
