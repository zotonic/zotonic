{% if is_connect and 'facebook'|member:identity_types %}
	<a id="{{ #fbdis }}" href="#disconnect" class="btn social-login" style="color: white; background-color: #44609d"><span class="icon-facebook-sign"></span> {_ Disconnect from Facebook _}</a>
	{% wire id=#fbdis 
			action={confirm title=_"Disconnect from Facebook" 
							text=_"Do you want to disconnect your Facebook account?"
							ok=_"Disconnect"
							action={auth_disconnect id=m.acl.user type="facebook"}
					}
	%}
{% else %}
	<a href="{% url logon_service service='facebook' is_connect=is_connect %}" class="btn social-login do_popupwindow" data-popupwindow="height:300" style="color: white; background-color: #44609d"><span class="fab fa-facebook"></span> {% if is_connect %}{_ Connect with Facebook _}{% else %}{_ Log on with Facebook _}{% endif %}</a>
{% endif %}
