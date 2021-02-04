{% extends "admin_base.tpl" %}

{% block title %}{_ Users _}{% endblock %}

{% block search_target %}{% url admin_user %}{% endblock %}

{% block search_placeholder %}{_ Search users _}{% endblock %}

{% block content %}
    {% with m.acl.is_allowed.use.mod_admin_identity as is_users_editable %}

    <div class="admin-header">
        <h2>
            {% if q.qs %}
                {_ Users _} {_ matching _} “{{ q.qs|escape }}”
            {% else %}
                {_ Users Overview _}
            {% endif %}
        </h2>
        <p>
            {_ Every page/person can be made into a user on the edit page. The difference between a user and a normal page is only that the former has logon credentials attached to its page record. _}
        </p>
    </div>

    {% if is_users_editable %}
        <div class="well z-button-row">
            {% button class="btn btn-primary" text=_"Make a new user" action={dialog_user_add on_success={reload}} %}
        </div>
    {% else %}
        <div class="alert alert-info">{_ You need to be an administrator to add users. _}</div>
    {% endif %}

    {% with m.acl.user as me %}

        <form method="GET" action="{% url admin_users %}">
            <label style="font-weight: normal; margin: 0; margin: 0 0 20px 0">
                <input type="hidden" name="qs" value="{{ q.qs|escape }}" />
                <input type="checkbox" name="persons" value="1" {% if q.persons %}checked="checked"{% endif %}
                    onchange="this.form.submit()" />
                {_ Also show persons without user account _}
            </label>
        </form>

        {% with m.search.paged[{users text=q.qs page=q.page users_only=not(q.persons)}] as result %}

            <table class="table table-striped do_adminLinkedTable">
                <thead>
                    <tr>
                        <th width="20%">{_ Name _}</th>
                        <th width="20%">{_ Username _}</th>
                        <th width="15%">{_ Created on _}</th>
                        <th width="40%">{_ Modified on _}</th>
                    </tr>
                </thead>

                <tbody>
                    {% for id in result %}
                        <tr id="{{ #tr.id }}" data-href="javascript:;" {% if not id.is_published %}class="unpublished"{% endif %}>
                            <td>{{ m.rsc[id].title|striptags }}</td>
                            <td>
                                {% if not m.identity[id].username %}
                                    &mdash;
                                {% else %}
                                    {{ m.identity[id].username|escape }}{% if id == me %}  <strong>{_ (that's you) _}</strong>{% endif %}</td>
                            {% endif %}
                            <td>{{ id.created|date:_"d M Y, H:i" }}</td>
                            <td>
                                {{ id.modified|date:_"d M Y, H:i" }}
                                <div class="pull-right buttons">
                                    {% if is_users_editable %}
                                        {% button class="btn btn-default btn-xs"
                                                  action={dialog_set_username_password id=id on_delete={slide_fade_out target=#tr.id}}
                                                  text=_"set username / password"
                                        %}
                                    {% endif %}
                                    {% button class="btn btn-default btn-xs" text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
                                </div>
                            </td>
                        </tr>
                        {% wire id=#tr.id|append:" td" action={dialog_edit_basics id=id target=undefined} %}
                    {% empty %}
                        <tr>
                            <td colspan="4">
                                {_ No users found. _}
                            </td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>

            {% pager result=result dispatch="admin_user" qargs hide_single_page %}

        {% endwith %}
    {% endwith %}

{% endwith %}
{% endblock %}
