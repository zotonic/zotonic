{% if not m.acl.user %}
	<p>{_ No account yet? _} <a href="{% url signup p=page %}" id="back_to_logon">{_ Sign up _}</a></p>
	{% if logon_state %}
	    {% wire id="back_to_logon"
            action={
                replace
                template=update_template
                target=update_target
                logon_state="signup"
                logon_context=logon_context
            }
        %}
	{% endif %}
{% endif %}



