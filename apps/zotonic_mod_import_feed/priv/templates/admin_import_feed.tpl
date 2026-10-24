{% extends "admin_base.tpl" %}

{% block title %}
    {_ Import feeds _}
{% endblock %}

{% block content %}
    {% if m.acl.use.mod_import_feed %}
        {% lib "css/admin_import_feed.css" %}
        <div class="admin-header">
            <h2>{_ Import feeds _}</h2>
            <p>{_ Discover RSS, Atom, JSON or iCalendar feeds and import their content automatically. _}</p>
        </div>
        <p><a class="btn btn-default" href="{% url admin_import_feed_log %}">{_ Activity log _}</a></p>
        {% wire id="feed-discover-form" type="submit" postback=`discover` delegate=`mod_import_feed`
            action={add_class target="feed-discover-form" class="is-loading"}
            action={disable selector=".import-feed-fetch-button"}
        %}
        <form id="feed-discover-form" method="post" action="postback" class="form-inline">
            <label for="feed-url">{_ Website or feed URL _}</label>
            <input id="feed-url" name="url" type="url" required class="form-control" placeholder="https://example.com/">
            <label for="feed-discover-auth">{_ Fetch using _}</label>
            <select id="feed-discover-auth" name="use_credentials" class="form-control">
                <option value="0">{_ Anonymous requests _}</option>
                <option value="1">{_ Registered OAuth2 credentials _}</option>
            </select>
            <button type="submit" class="btn btn-primary import-feed-fetch-button">{_ Discover feeds _}</button>
            <span class="import-feed-discover-loading" role="status" aria-live="polite">
                <img src="/lib/images/spinner.gif" width="16" height="16" alt="">
                {_ Checking website for feeds… _}
            </span>
        </form>

        <div id="feed-discovery">
        </div>
        <hr>
        <div class="table-responsive">
            <table class="table table-striped">
                <thead>
                    <tr>
                        <th>{_ Feed _}</th>
                        <th>{_ Status _}</th>
                        <th>{_ Articles _}</th>
                        <th>{_ Polls _}</th>
                        <th>{_ Imported / updated / skipped _}</th>
                        <th>{_ Errors _}</th>
                        <th>{_ Last poll _}</th>
                        <th>{_ Next poll _}</th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>
                    {% for f in m.import_feed.list %}
                        <tr>
                            <td>
                                <a href="{% url admin_import_feed_edit id=f.id %}">{{ f.title|escape }}</a>
                                <br>
                                <small>{{ f.url|escape }}</small>
                            </td>
                            <td>
                                {% if f.is_paused %}
                                    {_ Paused _}
                                {% elseif f.is_running %}
                                    {_ Importing _}
                                {% else %}
                                    {_ Active _}
                                {% endif %}
                            </td>
                            <td>{{ f.article_count }}</td>
                            <td>{{ f.polls }}</td>
                            <td>{{ f.imported }} / {{ f.updated }} / {{ f.skipped }}</td>
                            <td>
                                {{ f.errors }}
                                {% if f.last_error %}
                                    <br>
                                    <small>{{ f.last_error|escape }}</small>
                                {% endif %}
                            </td>
                            <td>{{ f.last_poll|date:"Y-m-d H:i" }}</td>
                            <td>
                                {% if not f.is_paused %}
                                    {{ f.next_poll|date:"Y-m-d H:i" }}
                                {% endif %}
                            </td>
                            <td>
                                <a class="btn btn-default btn-xs" href="{% url admin_import_feed_log feed=f.id %}">{_ Log _}</a>
                                <a class="btn btn-default btn-xs" href="{% url admin_import_feed_edit id=f.id %}">{_ Manage _}</a>
                            </td>
                        </tr>
                    {% empty %}
                        <tr>
                            <td colspan="9">{_ No feeds configured yet. _}</td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>

        </div>
    {% endif %}
{% endblock %}
