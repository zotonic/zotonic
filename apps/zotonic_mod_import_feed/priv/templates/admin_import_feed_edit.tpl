{% extends "admin_base.tpl" %}

{% block title %}
    {_ Feed settings _}
{% endblock %}

{% block content %}
    {% with m.import_feed.get[q.id] as feed %}
        {% if feed %}
            <div class="admin-header">
                <h2>{{ feed.title|escape }}</h2>
                <p>
                    <a href="{% url admin_import_feed %}">{_ All feeds _}</a>
                </p>
            </div>
            <a class="btn btn-default" href="{% url admin_import_feed_log feed=feed.id %}">{_ Activity log _}</a>
            {% wire id="feed-pause" postback={pause id=feed.id paused=not feed.is_paused} delegate=`mod_import_feed` %}
            <button id="feed-pause" class="btn btn-default">
                {% if feed.is_paused %}
                    {_ Resume _}
                {% else %}
                    {_ Pause _}
                {% endif %}
            </button>
            {% wire id="feed-test" postback={test id=feed.id} delegate=`mod_import_feed`
                action={add_class target="feed-preview" class="is-loading"}
                action={disable target="feed-test"}
            %}
            <button id="feed-test" class="btn btn-default">{_ Test / preview _}</button>
            {% if not feed.is_paused %}
                {% wire id="feed-poll" postback={poll id=feed.id} delegate=`mod_import_feed` %}
                <button id="feed-poll" class="btn btn-default">{_ Poll now _}</button>
                {% wire id="feed-reimport" postback={poll id=feed.id reimport} delegate=`mod_import_feed` %}
                <button id="feed-reimport" class="btn btn-default">{_ Reimport current entries _}</button>
            {% endif %}
            <p class="help-block">{_ Testing fetches example entries without importing them. Reimport processes the entries currently available in the feed. Deleted resources stay deleted. _}</p>
            {% if feed.last_error %}
                <p class="alert alert-danger">{{ feed.last_error|escape }}</p>
            {% endif %}
            {% include "_admin_import_feed_edit.tpl" %}
            <h3>{_ Import history _}</h3>
            <p>{_ Articles and dependent media, 100 entries per page. _}</p>
            {% with m.import_feed.history[feed.id][q.offset|default:0] as history %}
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th>{_ Resource _}</th>
                            <th>{_ Source identity _}</th>
                            <th>{_ Kind _}</th>
                            <th>{_ Status _}</th>
                            <th>{_ Last processed _}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {% for item in history.items %}
                            <tr>
                                <td>
                                    {% if item.rsc_id %}
                                        <a href="{% url admin_edit_rsc id=item.rsc_id %}">{{ item.rsc_id.title|default:item.rsc_id }}</a>
                                    {% else %}
                                        —
                                    {% endif %}
                                </td>
                                <td>{{ item.uri|escape }}</td>
                                <td>{{ item.kind|escape }}</td>
                                <td>
                                    {{ item.status|escape }}
                                    {% if item.last_error %}
                                        <br>
                                        {{ item.last_error|escape }}
                                    {% endif %}
                                </td>
                                <td>{{ item.modified|date:"Y-m-d H:i" }}</td>
                            </tr>
                        {% empty %}
                            <tr>
                                <td colspan="5">{_ Nothing imported yet. _}</td>
                            </tr>
                        {% endfor %}
                    </tbody>
                </table>

                <nav aria-label="{_ Import history pages _}">
                    {% if history.offset %}
                        <a class="btn btn-default" href="{% url admin_import_feed_edit id=feed.id offset=history.previous %}">{_ Previous _}</a>
                    {% endif %}
                    {% if history.has_next %}
                        <a class="btn btn-default" href="{% url admin_import_feed_edit id=feed.id offset=history.next %}">{_ Next _}</a>
                    {% endif %}
                </nav>
            {% endwith %}
        {% else %}
            <p class="alert alert-warning">{_ Feed not found or access denied. _}</p>
        {% endif %}
    {% endwith %}
{% endblock %}
