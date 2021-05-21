{% if is_connect and 'microsoft'|member:identity_types %}
    <a id="{{ #fbdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #0068b8"><span class="fab fa-windows"></span> {_ Disconnect from Microsoft _}</a>
    {% wire id=#fbdis
            action={confirm title=_"Disconnect from Microsoft"
                            text=_"Do you want to disconnect your Microsoft account?"
                            ok=_"Disconnect"
                            action={auth_disconnect id=m.acl.user type="microsoft"}
                    }
    %}
{% else %}
    <a href="{% url microsoft_authorize is_connect=is_connect %}"
       class="btn z-btn-social"
       style="background-color: #0068b8"
       data-onclick-topic="model/window/post/open"
    >
       <span class="fab fa-windows"></span>
        {% if is_connect %}{_ Connect with Microsoft _}
        {% else %}{_ Log in with Microsoft _}{% endif %}
    </a>
{% endif %}
