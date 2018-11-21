{% if m.config.mod_twitter.consumer_key.value and m.config.mod_twitter.useauth.value %}
<li id="logon_twitter">
	{% if is_connect and 'twitter'|member:identity_types %}
		<a id="{{ #twdis }}" href="#disconnect" class="btn social-login" style="color: white; background-color: #55acee"><span class="icon-twitter-sign"></span> {_ Disconnect from Twitter _}</a>
		{% wire id=#twdis 
				action={confirm title=_"Disconnect from Twitter" 
								text=_"Do you want to disconnect your Twitter account?"
								ok=_"Disconnect"
								action={auth_disconnect id=m.acl.user type="twitter"}
						}
		%}
	{% else %}
		<a href="{% url logon_service service='twitter' is_connect=is_connect %}" class="btn social-login do_popupwindow" style="color: white; background-color: #55acee"><span class="fab fa-twitter"></span> {% if is_connect %}{_ Connect with Twitter _}{% else %}{_ Log on with Twitter _}{% endif %}</a>
	{% endif %}
</li>
{% endif %}
