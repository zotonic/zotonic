{% if m.config.mod_instagram.consumer_key.value %}
<li id="logon_instagram">
	{% if is_connect and 'instagram'|member:identity_types %}
		<a id="{{ #twdis }}" href="#disconnect" class="btn social-login" style="color: white; background-color: #517fa4"><span class="icon-instagram"></span> {_ Disconnect from Instagram _}</a>
		{% wire id=#twdis 
				action={confirm title=_"Disconnect from Instagram" 
								text=_"Do you want to disconnect your Instagram account?"
								ok=_"Disconnect"
								action={auth_disconnect id=m.acl.user type="instagram"}
						}
		%}
	{% else %}
		<a href="{% url logon_service service='instagram' is_connect=is_connect %}" class="btn social-login do_popupwindow" style="color: white; background-color: #517fa4"><span class="icon-instagram"></span> {% if is_connect %}{_ Connect with Instagram _}{% else %}{_ Log on with Instagram _}{% endif %}</a>
	{% endif %}
</li>
{% endif %}
