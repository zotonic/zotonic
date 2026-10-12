{% if m.acl.use.mod_import_feed %}
    {% if m.import_feed.can_select_user %}
        <input type="hidden" name="user_id" value="{{ user_id|escape }}">
    {% endif %}
    <div class="well well-sm import-feed-user">
        <i class="z-icon z-icon-user text-muted" aria-hidden="true"></i>
        <div class="import-feed-user-details">
            {% if user_id %}
                <strong>{{ m.rsc[user_id].title|default:user_id }}</strong>
                {% if m.import_feed.can_select_user %}
                    {% with m.identity[user_id].username as username %}
                        {% if username %}
                            <small class="text-muted import-feed-user-username">{{ username|escape }}</small>
                        {% endif %}
                    {% endwith %}
                {% endif %}
            {% else %}
                <span class="text-muted">{_ No import user selected _}</span>
            {% endif %}
        </div>
        {% if m.import_feed.can_select_user %}
            {% if user_id %}
                {% button class="btn btn-default btn-sm" text=_"Change user"
                          postback={select_user} delegate=`mod_import_feed` %}
            {% else %}
                {% button class="btn btn-default btn-sm" text=_"Select user"
                          postback={select_user} delegate=`mod_import_feed` %}
            {% endif %}
        {% endif %}
    </div>
    {% if user_id and m.import_feed.user_is_admin[user_id] %}
        <p class="alert alert-danger" role="alert">{_ Administrator accounts cannot run feed imports. Importing external content with full administrator permissions is dangerous. Select a non-administrator account with only the permissions needed for importing. _}</p>
    {% endif %}
{% endif %}
