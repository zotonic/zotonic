{#
Logon screens in a modal dialog.

Changes relative to the logon *page*:
* The page title for the sign in form is omitted. A (generic) title is shown in the dialog header.
* Forms are posted using a wire postback because we cannot use controller_logon.
* State is maintained to switch between screens. This is used to replace the modal contents, instead of loading a page at a dispatch url.
   * The variable 'logon_state' is used in external templates (for instance '_logon_link.tpl') to create wired update links. States: 'logon', 'signup', 'reset', 'reminder'
   * The variable 'logon_context' is used to distinguish user login from admin login. States: 'admin_logon' or empty.

Make sure that these CSS files are loaded:

    "css/z.icons.css"
    "css/logon.css"
    
Loads the template according to the state:
- logon
- signup
- reset
#}
<div id="z_logon_or_signup">
{% with
    1,
    logon_state|default:"logon",
    logon_context,
    logon_context|default:"",
    "z_logon_or_signup",
    "_logon_modal.tpl"
    as
    use_wire,
    logon_state,
    original_logon_context,
    logon_context,
    update_target,
    update_template
%}
{% if logon_state == `signup` %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_signup_title.tpl"
        logon_form_entry_tpl="_signup_form.tpl"
        logon_form_support_tpl="_signup_support.tpl"
        logon_state=logon_state
        logon_context=logon_context
        update_target=update_target
        update_template=update_template
        use_wire=use_wire
    %}
{% elseif logon_state == `reminder` %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_logon_reminder_title.tpl"
        logon_form_entry_tpl="_logon_reminder_form.tpl"
        logon_form_support_tpl="_logon_reminder_support.tpl"
        logon_state=logon_state
        logon_context=logon_context
        update_target=update_target
        update_template=update_template
        use_wire=use_wire
    %}
{% elseif logon_state == `reset` %}
    {% include "_logon_form.tpl"
        logon_form_title_tpl="_logon_reset_title.tpl"
        logon_form_entry_tpl="_logon_reset_form.tpl"
        logon_form_support_tpl="_logon_reset_support.tpl"
        logon_state=logon_state
        logon_context=logon_context
        update_target=update_target
        update_template=update_template
        use_wire=use_wire
    %}
{% else %}
    {% if logon_context == 'admin_logon' %}
        {% include "_logon_form.tpl"
            logon_form_entry_tpl="_logon_login_form.tpl"
            logon_form_support_tpl="_logon_login_support.tpl"
            logon_state=logon_state
            logon_context=logon_context
        %}
    {% else %}
        {% include "_logon_form.tpl"
            logon_form_extra_tpl="_logon_login_extra.tpl"
            logon_form_entry_tpl="_logon_login_form.tpl"
            logon_form_support_tpl="_logon_login_support.tpl"
            logon_form_outside_tpl="_logon_login_outside.tpl"
            logon_state=logon_state
            logon_context=logon_context
            update_target=update_target
            update_template=update_template
            use_wire=use_wire
        %}
    {% endif %}
{% endif %}

{#
Hook into back buttons that we cannot reach without passing variables through forms and controller_logon:
#}
{% wire
    name="stage_back_to_logon"
    action={
        replace
        template=update_template
        target=update_target
        logon_state="logon"
        logon_context=original_logon_context
    }
%}
{% wire
    name="email_reminder"
    action={
        replace
        template=update_template
        target=update_target
        logon_state="reminder"
        logon_context=original_logon_context
    }
%}

{% javascript %}
    var $container = $("#z_logon_or_signup");
    var wire = function(name, id) {
        if (!$container.data(name)) {
            $container.data(name, function(e) {
                e.preventDefault();
                z_event(name);
            });
            $container.on("click", id, $container.data(name));
        }
    }
    wire("stage_back_to_logon", "#stage_back_to_logon");
    wire("email_reminder", "#logon_error_link_reminder");
{% endjavascript %}
{% endwith %}
</div>


