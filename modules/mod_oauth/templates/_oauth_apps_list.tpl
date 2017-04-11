<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th>{_ Title _}</th>
            <th>{_ Access to _}</th>
            <th>{_ Consumer key _}</th>
            <th>{_ Actions _}</th>
        </tr>
    </thead>

    <tbody>
        {% for app in m.oauth_app %}
            {% with app.id as id %}
                <tr id="{{ #tr.id }}" title="{{ app.application_descr|escape }}" data-href="#">
                    <td>{{ app.application_title|escape }}</td>
                    <td>
                        {% for perm in m.oauth_perms.humanreadable[app.id] %}
                            {{ perm.desc|escape }}{% if not forloop.last %},{% endif %}
                        {% empty %}
                            &nbsp;
                        {% endfor %}
                    </td>
                    <td>
                        <tt>{{ app.consumer_key }}</tt>
                    </td>
                    <td>
                        {% button class="btn btn-default btn-xs" text=_"Edit" postback={start_edit_app id=id} title=_"Change the title, description and access permissions of this application." %}
                        <a class="btn btn-default btn-xs" href="{% url admin_oauth app_id=id %}" title=_"Show applications/users that are using this key">{_ Users _}</a>
                        {% button class="btn btn-default btn-xs" text=_"Delete" postback={start_del_app id=id} %}
                    </td>
                </tr>
                {% wire id=#tr.id action={redirect dispatch=`admin_oauth` app_id=id} %}
            {% endwith %}
        {% empty %}
            <tr>
                <td colspan="4">
                    {_ No applications registered yet. _}
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
