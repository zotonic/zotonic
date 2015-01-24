{#
Loads the template according to the dispatch:
- logon
- logon/reminder
- logon/reset
#}
{% if zotonic_dispatch == `logon_reminder` %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_logon_reminder_title.tpl"
        logon_form_entry_tpl="_logon_reminder_form.tpl"
        logon_form_support_tpl="_logon_reminder_support.tpl"
    %}
{% elseif zotonic_dispatch == `logon_reset` %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_logon_reset_title.tpl"
        logon_form_entry_tpl="_logon_reset_form.tpl"
        logon_form_support_tpl="_logon_reset_support.tpl"
    %}
{% elseif zotonic_dispatch == `admin_logon` %}
    {% include "_logon_form.tpl"
        logon_form_entry_tpl="_logon_login_form.tpl"
        logon_form_support_tpl="_logon_login_support.tpl"
    %}
{% else %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_logon_login_title.tpl"
        logon_form_extra_tpl="_logon_login_extra.tpl"
        logon_form_entry_tpl="_logon_login_form.tpl"
        logon_form_support_tpl="_logon_login_support.tpl"
        logon_form_outside_tpl="_logon_login_outside.tpl"
    %}
{% endif %}