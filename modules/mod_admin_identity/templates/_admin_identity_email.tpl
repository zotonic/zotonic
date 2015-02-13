{% with m.identity[id].all.email as idns %}
<label class="control-label" for="{{ #email }}">{_ E-mail address _}</label>
<div id="{{ #email_idns }}">
	<div id="{{ #email_list }}">
		{% include "_identity_verify_table.tpl" identities=idns %}
	</div>

    <div id="{{ #email_add_group }}" class="input-group">
        <input id="{{ #email }}" type="email" name="idn-key" value="{% if not idns %}{{ id.email }}{% endif %}" placeholder="{_ Add e-mail address _}" class="nosubmit form-control" />
        <span class="input-group-btn">
            <button id="{{ #email_add }}" class="btn btn-default" type="button">{_ Add _}</button>
        </span>
    </div>

</div>
{% endwith %}

{% wire id=#email_add
	action={mask target=#email_idns}
    postback={identity_add id=id type=`email` input=#email error_target=#email_add_group}
    delegate=`mod_admin_identity`
    qarg=#email
%}
