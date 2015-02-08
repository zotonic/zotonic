{% if not m.acl.user %}
	<p>{_ No account yet? _} <a href="{% url signup p=page %}" id="go_to_signup">{_ Sign up _}</a></p>
	{% if logon_state %}
	    {% wire id="back_to_logon"
            action={
                replace
                template=update_template
                target=update_target
                logon_state="signup"
                logon_context=logon_context
                style_boxed=style_boxed
                style_width=style_width
            }
        %}
	{% endif %}
{% endif %}



