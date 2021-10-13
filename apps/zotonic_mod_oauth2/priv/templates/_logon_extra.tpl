{% for app in m.oauth2_consumer.consumers.auth %}
    {% with app.id as app_id %}
        {% if is_connect and m.oauth2_consumer.is_connected[app.name] %}
            <a id="{{ #oauthdis.app_id }}"
               href="#disconnect"
               class="btn z-btn-social"
               style="background-color: #111"
            >
                {_ Disconnect from _} {{ app.description|escape }}
            </a>
            {% wire id=#oauthdis.app_id
                    action={confirm title=[_"Disconnect ", app.description|escape]
                                    text=_"Do you want to disconnect your account?"
                                    ok=_"Disconnect"
                                    action={auth_disconnect id=m.acl.user type="mod_oauth2" keyprefix=a.name}
                            }
            %}
        {% elseif (not is_connect and app.is_use_auth)
               or (    is_connect and app.is_use_import)
         %}
            <a href="{% url oauth2_consumer_authorize is_connect=is_connect consumer_id=app.id %}"
               class="btn z-btn-social"
               style="background-color: #111"
               data-onclick-topic="model/window/post/open"
            >
               <span class="z-icon z-icon-"></span>
               {% if is_connect %}{_ Connect with _} {{ app.description|escape }}
               {% else %}{_ Log in with _} {{ app.description|escape }}{% endif %}
            </a>
        {% endif %}
    {% endwith %}
{% endfor %}
