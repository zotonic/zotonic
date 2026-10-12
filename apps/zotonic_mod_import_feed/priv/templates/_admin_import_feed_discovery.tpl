{% if m.acl.use.mod_import_feed %}
    <h3>{_ Select a feed _}</h3>
    <ul class="list-unstyled">
        {% for f in feeds %}
            {% with forloop.counter as index %}
                <li>
                    {% wire id=#select.index postback={select url=f.url use_credentials=use_credentials} delegate=`mod_import_feed` %}
                    <button id="{{ #select.index }}" class="btn btn-default">{{ f.title|escape }}</button>
                    <small>{{ f.url|escape }}</small>
                </li>
            {% endwith %}
        {% endfor %}
    </ul>
{% endif %}
