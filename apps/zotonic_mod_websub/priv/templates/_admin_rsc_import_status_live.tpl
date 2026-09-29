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
                &nbsp; <span title="{{ subscription.last_received|date:"Y-m-d H:i" }}">
                    {_ Last update received: _}
                    {% if subscription.last_received >= now|sub_day:7 %}
                        {{ subscription.last_received|timesince }}
                    {% else %}
                        {{ subscription.last_received|date:"Y-m-d H:i" }}
                    {% endif %}
                </span>
            {% endif %}
            {% if subscription.last_error or subscription.credential_error %}
                {% include "_websub_error.tpl" error=subscription.last_error|default:subscription.credential_error %}
            {% endif %}
        </div>
    {% endwith %}
{% endif %}
