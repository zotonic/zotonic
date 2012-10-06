{% if m.config.mod_facebook.useauth.value and m.config.mod_facebook.appid.value %}
<li id="logon_facebook">
	{% include "_facebook_login_link.tpl" %}
</li>
{% endif %}
