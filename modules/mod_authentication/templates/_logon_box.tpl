{#
Params:
- page
- error_reason
- style_boxed -- creates a colored background box
- form_title_tpl
- form_extra_tpl
- form_form_tpl
- form_support_tpl
- form_outside_tpl
#}
<div id="signup_logon_box" class="z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}" {% if style_width %}style="width: {{ style_width }};"{% endif %}>
    {% if form_title_tpl %}
        {% include form_title_tpl %}
    {% endif %}

    <div class="z-logon-form">
        {% if m.rsc.page_logon.body %}
            <div class="alert">{{ m.rsc.page_logon.body }}</div>
        {% endif %}

        {% if form_extra_tpl %}
            {% include form_extra_tpl %}
        {% endif %}

        <div id="logon_error" class="alert alert-danger">
            {% include "_logon_error.tpl" reason=error_reason %}
        </div>

        {% if form_form_tpl %}
            {% include form_form_tpl
                page=page
                use_wire=use_wire
                form_fields_tpl=form_fields_tpl
                style_boxed=style_boxed
                style_width=style_width
            %}
        {% endif %}

        {% if form_support_tpl %}
            {% include form_support_tpl
                update_target=update_target
                update_template=update_template
                logon_state=logon_state
                logon_context=logon_context
                style_boxed=style_boxed
                style_width=style_width
            %}
        {% endif %}
    </div>

    {% if form_outside_tpl %}
        {% include form_outside_tpl
            update_target=update_target
            update_template=update_template
            logon_state=logon_state
            logon_context=logon_context
            style_boxed=style_boxed
            style_width=style_width
        %}
    {% endif %}

</div>
{# Use a real post for all forms on this page, and not AJAX or Websockets. This will enforce all cookies to be set correctly. #}
{% javascript %}
z_only_post_forms = true;
{% endjavascript %}

{# Set a listener on the session changes - needed for logon via external auth methods or via other pages #}
{% javascript %}
    z_transport_delegate_register('reload', function(_status) {
        $('body').mask();
        z_transport("controller_logon", "ubf", { msg: "logon_redirect", page: '{{ page|default:q.p|escapejs }}' });
    });
{% endjavascript %}
