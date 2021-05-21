{% if m.twitter.useauth %}
<li id="logon_twitter">
	{% if is_connect and 'twitter'|member:identity_types %}
		<a id="{{ #twdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #55acee"><span class="z-icon z-icon-twitter"></span> {_ Disconnect from Twitter _}</a>
		{% wire id=#twdis
				action={confirm title=_"Disconnect from Twitter"
								text=_"Do you want to disconnect your Twitter account?"
								ok=_"Disconnect"
								action={auth_disconnect id=m.acl.user type="twitter"}
						}
		%}
	{% else %}
		<a href="{% url twitter_authorize is_connect=is_connect %}"
		   class="btn z-btn-social"
		   style="background-color: #55acee"
		   data-onclick-topic="model/window/post/open"
		>
		   <span class="z-icon z-icon-twitter"></span>
			{% if is_connect %}{_ Connect with Twitter _}
			{% else %}{_ Log in with Twitter _}{% endif %}
		</a>
	{% endif %}
</li>
{% endif %}
