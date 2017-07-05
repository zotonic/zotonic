{% if m.acl.user %}
	{% with m.identity[m.acl.user].all_types as idn_types %}
		<ul class="list-unstyled social-login-list">
			{% all include "_logon_extra.tpl" is_connect identity_types=idn_types %}
		</ul>
	{% endwith %}
{% endif %}
