{#
Params:
page
error_reason
xs_props
style_boxed -- creates a colored background box
form_title_tpl
form_extra_tpl
form_form_tpl
form_support_tpl
form_outside_tpl
#}
<div id="signup_logon_box" class="z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}" {% if style_width %}style="width: {{ style_width }};"{% endif %}>
    {% if form_title_tpl %}
        {% include form_title_tpl %}
    {% endif %}

    <div class="z-logon-form">
        {% if form_extra_tpl %}
            {% include form_extra_tpl xs_props=xs_props %}
        {% endif %}

        <div id="signup_error_duplicate_identity" class="alert alert-danger">
            {_ Sorry, there is already an account coupled to your account at your service provider. Maybe your account here was suspended. _}
        </div>

        {% if form_form_tpl %}
            {% include form_form_tpl
                page=page
                use_wire=use_wire
                signup_delegate=signup_delegate
                form_fields_tpl=form_fields_tpl
                show_signup_name_title=show_signup_name_title
                show_signup_name_first=show_signup_name_first
                show_signup_name_prefix=show_signup_name_prefix
                show_signup_name_last=show_signup_name_last
                show_signup_name_email=show_signup_name_email
                show_signup_username_title=show_signup_username_title
                show_signup_username=show_signup_username
                show_signup_password=show_signup_password
                show_signup_password2=show_signup_password2
                show_signup_tos_title=show_signup_tos_title
                show_signup_tos_info=show_signup_tos_info
                show_signup_tos_checkbox=show_signup_tos_checkbox
            %}
        {% endif %}

        {% if form_support_tpl %}
            {% include form_support_tpl
                update_target=update_target
                update_template=update_template
                logon_state=logon_state
                logon_context=logon_context
            %}
        {% endif %}
    </div>

    {% if form_outside_tpl %}
        {% include form_outside_tpl
            update_target=update_target
            update_template=update_template
            logon_state=logon_state
            logon_context=logon_context
        %}
    {% endif %}
</div>

