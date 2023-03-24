{% extends "page.tpl" %}

{% block title %}{_ Verify Address | _}{% endblock %}

{% block content %}
{% with q.idn_id|to_integer as idn_id %}
<div class="padding" style="max-width: 400px; margin: 10px auto;">

	<div id="verify-checking">
		<h1>{_ Verifying... _}</h1>

		<p><img src="/lib/images/spinner.gif" width="16" height="16" /> {_ One moment, please... _}</p>
	</div>

	<div id="verify-ok" style="display: none">
		<h1>{_ Thank you _}</h1>

		<p>{% if idn.type == 'email' %}{_ Your e-mail address is now verified. _}{% else %}{_ Your address is now verified. _}{% endif %}</p>

		<p>
			<a class="btn btn-primary" href="/">{_ Home _}</a>
			{% if m.acl.user %}<a class="btn btn-primary" href="{{ m.acl.user.page_url }}">{_ My page _}</a>{% endif %}
		</p>
	</div>

	<div id="verify-error" class="alert" style="display: none">
		<h1>{_ Sorry _}</h1>

		<p>{_ This verification key is unknown. _}</p>

		<p>
			<a class="btn btn-primary" href="/">{_ Home _}</a>
		</p>
	</div>

	{% wire postback={identity_verify_check verify_key=q.verify_key idn_id=idn_id} delegate=`mod_admin_identity` %}
</div>
{% endwith %}
{% endblock %}
