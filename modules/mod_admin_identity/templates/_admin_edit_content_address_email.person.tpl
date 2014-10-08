{% with m.identity[id].all.email as idns %}
<label class="control-label" for="{{ #email }}">{_ E-mail address _}</label>
<div>
	<div id="{{ #email_list }}">
		{% include "_identity_verify_table.tpl" identities=idns %}
	</div>

	<div class="input-append">
		<input id="{{ #email }}" type="email" name="idn-key" value="{% if not idns %}{{ id.email }}{% endif %}" placeholder="{_ Add e-mail address _}" class="input-xlarge nosubmit form-control" />
		<a id="{{ #email_add }}" href="#" class="btn">{_ Add _}</a>
	</div>
	{% validate id=#email type={email failure_message=""} %}
</div>
{% endwith %}

{% wire id=#email_add
		postback={identity_add id=id type=`email` list=#email_list input=#email}
		delegate=`mod_admin_identity`
		qarg=#email
%}
