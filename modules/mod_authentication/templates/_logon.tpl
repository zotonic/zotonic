{#
Loads the template according to the dispatch:
- logon
- logon/reminder
- logon/reset
- admin_logon

Params:
logon_context: used to distinguish user login from admin login; values: 'admin_logon' or empty
style_boxed: creates a background around the form
style_width: width of box

#}
{% with     logon_context|if_undefined:(zotonic_dispatch=='admin_logon')|if:"admin_logon":""
    as
    logon_context
%}
{% if zotonic_dispatch == `logon_reminder` %}
    {% include "_logon_box.tpl"
        form_title_tpl="_logon_reminder_title.tpl"
        form_form_tpl="_logon_reminder_form.tpl"
        form_fields_tpl="_logon_reminder_form_fields.tpl"
        form_support_tpl="_logon_reminder_support.tpl"
        style_boxed=style_boxed
        style_width=style_width
        logon_context=logon_context
    %}
{% elseif zotonic_dispatch == `logon_reset` %}
    {% include "_logon_box.tpl"
        form_title_tpl="_logon_reset_title.tpl"
        form_form_tpl="_logon_reset_form.tpl"
        form_fields_tpl="_logon_reset_form_fields.tpl"
        form_support_tpl="_logon_reset_support.tpl"
        style_boxed=style_boxed
        style_width=style_width
        logon_context=logon_context
    %}
{% elseif zotonic_dispatch == `admin_logon` %}
    {# hide title and social login #}
    {% include "_logon_box.tpl"
        form_form_tpl="_logon_login_form.tpl"
        form_fields_tpl="_logon_login_admin_form_fields.tpl"
        form_support_tpl="_logon_login_support.tpl"
        style_boxed=style_boxed
        style_width=style_width
        logon_context=logon_context
    %}
{% else %}
    {% include "_logon_box.tpl"
        form_title_tpl="_logon_login_title.tpl"
        form_extra_tpl="_logon_login_extra.tpl"
        form_form_tpl="_logon_login_form.tpl"
        form_fields_tpl="_logon_login_form_fields.tpl"
        form_support_tpl="_logon_login_support.tpl"
        form_outside_tpl="_logon_login_outside.tpl"
        style_boxed=style_boxed
        style_width=style_width
        logon_context=logon_context
    %}
{% endif %}
{% endwith %}