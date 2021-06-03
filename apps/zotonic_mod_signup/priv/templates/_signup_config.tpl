{#
Params (optionally) passed from logon modal:
logon_state
logon_context
update_target
update_template
style_boxed
style_width
#}

{# non-critical values, may be changed #}
{% with
    show_signup_name_title|default_if_none:1,
    show_signup_username_title|default_if_none:1,
    show_signup_password2|default_if_none:0,
    show_signup_tos_title|default_if_none:1,
    show_signup_tos_info|default_if_none:1,
    style_boxed|default_if_none:0,
    style_width
as
    show_signup_name_title,
    show_signup_username_title,
    show_signup_password2,
    show_signup_tos_title,
    show_signup_tos_info,
    style_boxed,
    style_width
%}
    {% include "_signup.tpl" %}
{% endwith %}
