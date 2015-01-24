<p>
    <a href="{% url logon %}" id="back_to_logon">{_ Back to logon form _}</a>
    {% if logon_state %}
	    {% wire id="back_to_logon"
            action={
                replace
                template=update_template
                target=update_target
                logon_state="logon"
                logon_context=logon_context
            }
        %}
	{% endif %}
</p>