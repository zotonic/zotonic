{% if m.import_feed.can_select_user %}

    {% with m.search[{users text=search_text users_only=true pagelen=50}] as users %}
        <ul class="list-unstyled">
            {% for id in users %}
                <li>
                    {% wire id=#select.id postback={choose_user id=id} delegate="mod_import_feed" %}
                    <button id="{{ #select.id }}" class="btn btn-default">{{ id.title | default:m.identity[id].username | default:id }}</button>
                    <small>{{ m.identity[id].username | escape }}</small>
                </li>
            {% empty %}
                <li>{_ No users found. _}</li>
            {% endfor %}
        </ul>
        <p class="help-block">{_ Up to 50 users are shown. Search by name to narrow the results. Disabled accounts cannot be selected. _}</p>
    {% endwith %}
{% endif %}
