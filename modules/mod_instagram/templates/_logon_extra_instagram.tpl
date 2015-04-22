{% with
    "#3f729b"
    as
    brand_color
%}
{% if m.config.mod_instagram.consumer_key.value %}
<li id="logon_instagram">
	{% if is_connect and 'instagram'|member:identity_types %}
		<a id="{{ #twdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #517fa4"><span class="z-icon z-icon-instagram"></span> {_ Disconnect from Instagram _}</a>
		{% wire id=#twdis 
				action={confirm title=_"Disconnect from Instagram" 
								text=_"Do you want to disconnect your Instagram account?"
								ok=_"Disconnect"
								action={auth_disconnect id=m.acl.user type="instagram"}
						}
		%}
	{% else %}
		<a href="{% url logon_service service='instagram' is_connect=is_connect %}" class="btn z-btn-social do_popupwindow" style="background-color: #517fa4"><span class="z-icon z-icon-instagram"></span> {% if is_connect %}{_ Connect with Instagram _}{% else %}{_ Log in with Instagram _}{% endif %}</a>
	{% endif %}
</li>
{% endif %}
{% endwith %}