    {% with m.import_feed.get[qfeed] as feed %}
        {% if feed %}
            <p class="help-block">
                <a href="{% url admin_import_feed_edit id=feed.id %}">{{ feed.title|escape }}</a>
                — {{ feed.url|escape }}
            </p>
            {% wire id="log-fetch" postback={log_fetch id=feed.id} delegate=`mod_import_feed` %}
            <button id="log-fetch" type="button" class="btn btn-default" {% if feed.is_paused or feed.is_running %}disabled{% endif %}>{_ Fetch now _}</button>
            {% if feed.is_paused %}
                <p class="help-block">{_ This feed is paused. Resume it in its settings before fetching. _}</p>
            {% else %}
                <p class="help-block">{_ Fetches and imports new or changed entries using the configured import user. _}</p>
            {% endif %}
        {% endif %}
    {% endwith %}
