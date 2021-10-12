{% for app in m.oauth2_consumer.consumers.auth %}
    <a href="{% url oauth2_consumer_authorize is_connect=is_connect consumer_id=app.id %}"
       class="btn z-btn-social"
       style="background-color: #111"
       data-onclick-topic="model/window/post/open"
    >
       <span class="z-icon z-icon-"></span>
       {% if is_connect %}{_ Connect with _} {{ app.description|escape }}
       {% else %}{_ Log in with _} {{ app.description|escape }}{% endif %}
    </a>
{% endfor %}

{#
<li id="logon_linkedin">
    {% if is_connect and 'linkedin'|member:identity_types %}
        <a id="{{ #lnkddis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #0077B5"><span class="z-icon z-icon-linkedin"></span> {_ Disconnect from LinkedIn _}</a>
        {% wire id=#lnkddis
                action={confirm title=_"Disconnect from LinkedIn"
                                text=_"Do you want to disconnect your LinkedIn account?"
                                ok=_"Disconnect"
                                action={auth_disconnect id=m.acl.user type="linkedin"}
                        }
        %}
    {% else %}
        <a href="{% url linkedin_authorize is_connect=is_connect %}"
           class="btn z-btn-social"
           style="background-color: #0077B5"
           data-onclick-topic="model/window/post/open"
        >
           <span class="z-icon z-icon-linkedin"></span>
            {% if is_connect %}{_ Connect with _}
            {% else %}{_ Log in with LinkedIn _}{% endif %}
        </a>
    {% endif %}
</li>
#}
