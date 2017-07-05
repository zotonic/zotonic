{#
Params:
style_boxed
style_width
#}
{% if m.acl.user %}
    {% wire
        action={
            redirect
            location=m.rsc[m.acl.user].page_url
        }
    %}
{% else %}
    {# critical form parts, must be available #}
    {% with
        1,1,1,1,1,1
    as
        show_signup_name_first,
        show_signup_name_last,
        show_signup_name_email,
        show_signup_username,
        show_signup_password,
        show_signup_tos_checkbox
    %}
    {% with
        form_title_tpl|if_undefined:"_signup_title.tpl",
        form_extra_tpl|if_undefined:"_signup_extra.tpl",
        form_form_tpl|if_undefined:"_signup_form.tpl",
        form_fields_tpl|if_undefined:"_signup_form_fields.tpl",
        form_support_tpl|if_undefined:"_signup_support.tpl",
        form_outside_tpl|if_undefined:"_signup_outside.tpl",
        signup_delegate|if_undefined:"controller_signup"
    as
        form_title_tpl,
        form_extra_tpl,
        form_form_tpl,
        form_fields_tpl,
        form_support_tpl,
        form_outside_tpl,
        signup_delegate
    %}
        {% include "_signup_box.tpl" %}
    {% endwith %}
    {% endwith %}
{% endif %}
