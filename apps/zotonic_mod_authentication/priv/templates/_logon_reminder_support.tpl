{% if not m.acl.user %}
<div class="z-logon-support">
    <p>
        <a href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Back to sign in _}</a>
{#
       <a href="{% url logon %}" id="back_to_logon">{_ Back to sign in _}</a>
       {% if logon_state %}
            {% wire id="back_to_logon"
                action={
                    replace
                    template=update_template
                    target=update_target
                    logon_state="logon"
                }
            %}
        {% endif %}
#}
    </p>
</div>
{% endif %}
