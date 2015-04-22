{% with
    "#0976b4"
    as
    brand_color
%}
{% if m.config.mod_linkedin.useauth.value and m.config.mod_linkedin.appid.value %}
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
		<a href="{% url logon_service service='linkedin' is_connect=is_connect  %}" class="btn z-btn-social do_popupwindow" style="background-color: #0077B5"><span class="z-icon z-icon-linkedin"></span> {% if is_connect %}{_ Connect with LinkedIn _}{% else %}{_ Log in with LinkedIn _}{% endif %}</a>
	{% endif %}
</li>
{% endif %}
{% endwith %}