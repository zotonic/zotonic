<div class="z-logon-support">
    <p>
        <a href="{% url logon_reminder %}" id="{{ #link_reset }}">{_ Forgot your password? _}</a>
        {% if logon_state %}
            {% wire id=#link_reset
                action={
                    replace
                    template=update_template
                    target=update_target
                    logon_state="reminder"
                    logon_context=logon_context
                    style_boxed=style_boxed
                    style_width=style_width
                }
            %}
        {% endif %}
    </p>
</div>
