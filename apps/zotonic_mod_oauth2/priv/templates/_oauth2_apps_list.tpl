{% for app in m.oauth2.apps %}
    {% with app.id as id %}
        <tr id="{{ #app.id }}" class="clickable">
            <td>{% if app.is_enabled %}√{% else %}-{% endif %}</td>
            <td>{{ app.description|escape }}</td>
            <td>
                {% if app.user_id %}
                    <a href="{% url admin_edit_rsc id=app.user_id %}">
                        {% include "_name.tpl" id=app.user_id %}
                        ({{ app.user_id }})
                    </a>
                {% endif %}
            </td>
            <td>{{ app.modified|date:_"d M Y, H:i" }}</td>
            <td>{{ app.created|date:_"d M Y, H:i" }}</td>
            <td>{{ app.token_count }}</td>
            <td>
                <button class="btn btn-default" id="{{ #edit.id }}">
                    {_ Edit App _}
                </button>
                {% wire id=#edit.id
                        action={dialog_open
                            template="_dialog_oauth2_app.tpl"
                            title=_"Edit OAuth2 App"
                            app_id=app.id
                        }
                %}
                <a class="btn btn-default" href="{% url admin_oauth2_apps_tokens appid=app.id %}">
                    {_ Access tokens _}
                </a>
            </td>
        </tr>
    {% endwith %}
{% endfor %}