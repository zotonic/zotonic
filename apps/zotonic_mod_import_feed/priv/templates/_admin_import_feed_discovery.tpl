{% if m.acl.use.mod_import_feed %}
    <h3>{_ Select a feed _}</h3>
    <p class="import-feed-select-loading" role="status" aria-live="polite">
        <img src="/lib/images/spinner.gif" width="16" height="16" alt="">
        {_ Loading feed and example entries… _}
    </p>
    <ul class="list-unstyled">
        {% for f in feeds %}
            {% with forloop.counter as index %}
                <li>
                    {% wire id=#select.index postback={select url=f.url use_credentials=use_credentials} delegate=`mod_import_feed`
                        action={add_class target="feed-discovery" class="is-loading"}
                        action={disable selector=".import-feed-fetch-button"}
                    %}
                    <button id="{{ #select.index }}" type="button" class="btn btn-default import-feed-fetch-button">{{ f.title|escape }}</button>
                    <small>{{ f.url|escape }}</small>
                </li>
            {% endwith %}
        {% endfor %}
    </ul>
{% endif %}
