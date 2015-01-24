<p>
    <a href="{% url logon_reminder %}" id="link_reset">{_ I forgot my password _}</a>
    {% if logon_state %}
	    {% wire id="link_reset"
            action={
                replace
                template=update_template
                target=update_target
                logon_state="reminder"
            }
        %}
	{% endif %}
</p>
