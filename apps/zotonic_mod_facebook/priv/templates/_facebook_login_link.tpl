{% if is_connect and 'facebook'|member:identity_types %}
	<a id="{{ #fbdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #3b5998"><span class="z-icon z-icon-facebook"></span> {_ Disconnect from Facebook _}</a>
	{% wire id=#fbdis
			action={confirm title=_"Disconnect from Facebook"
							text=_"Do you want to disconnect your Facebook account?"
							ok=_"Disconnect"
							action={auth_disconnect id=m.acl.user type="facebook"}
					}
	%}
{% else %}
    <a href="{% url facebook_authorize is_connect=is_connect %}"
       class="btn z-btn-social"
       style="background-color: #3b5998"
       data-onclick-topic="model/window/post/open"
    >
       <span class="z-icon z-icon-facebook"></span>
        {% if is_connect %}{_ Connect with Facebook _}
        {% else %}{_ Log in with Facebook _}{% endif %}
    </a>
{% endif %}
