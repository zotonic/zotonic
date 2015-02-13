{% with m.identity[id].all.email as idns %}
<label class="control-label" for="{{ #email }}">{_ E-mail address _}</label>
<div class="controls" id="{{ #email_idns }}">
	{% include "_identity_verify_table.tpl" identities=idns %}

	<div class="input-append">
		<input id="{{ #email }}" type="email" name="idn-key" value="{% if not idns %}{{ id.email }}{% endif %}" placeholder="{_ Add e-mail address _}" class="input-xlarge nosubmit" />
		<a id="{{ #email_add }}" href="#" class="btn">{_ Add _}</a>
	</div>
	{% validate id=#email type={email failure_message=""} %}
</div>
{% endwith %}

{% wire id=#email_add
		action={mask target=#email_idns}
		postback={identity_add id=id type=`email` input=#email}
		delegate=`mod_admin_identity`
		qarg=#email
%}
