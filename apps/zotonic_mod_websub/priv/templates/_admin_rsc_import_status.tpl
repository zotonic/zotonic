{% if id.is_editable and not id.is_authoritative %}
    {% with m.websub.status[id] as subscription %}
        <div>
            {% if subscription.is_enabled %}
                {% if subscription.is_active %}
                    {_ Automatic updates are active. _}
                {% else %}
                    {_ Waiting for automatic updates to be confirmed. _}
                {% endif %}
                <button type="button" class="btn btn-default btn-xs" id="{{ #stop }}">{_ Stop automatic updates _}</button>
                {% wire id=#stop postback={subscription_stop id=id} delegate=`mod_websub` %}
            {% else %}
                {_ Automatic updates are stopped. _}
                <button type="button" class="btn btn-default btn-xs" id="{{ #start }}">{_ Start automatic updates _}</button>
                {% wire id=#start postback={subscription_start id=id} delegate=`mod_websub` %}
            {% endif %}
            {% if subscription.last_received %}
                <span>{_ Last update received: _} {{ subscription.last_received|date:"Y-m-d H:i" }}</span>
            {% endif %}
            {% if subscription.last_error or subscription.credential_error %}
                <p>{_ Automatic update error: _} {{ subscription.last_error|default:subscription.credential_error|escape }}</p>
            {% endif %}
        </div>
    {% endwith %}
{% endif %}
