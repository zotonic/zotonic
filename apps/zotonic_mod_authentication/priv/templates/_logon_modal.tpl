{#
Logon screens in a modal dialog.

Params:
logon_state: manages state instead of dispatch rule; values: logon, signup, reset
logon_context: used to distinguish user login from admin login; values: 'admin_logon' or empty
style_boxed: creates a background around the form
style_width: width of box
page: page to redirect to after succesful logon


Changes relative to the logon *page*:
* The page title for the sign in form is omitted. A (generic) title is shown in the dialog header.
* Forms are posted using a wire postback because we cannot use controller_logon.
* State is maintained to switch between screens. This is used to replace the modal contents, instead of loading a page at a dispatch url.
   * The variable 'logon_state' is used in external templates (for instance '_logon_link.tpl') to create wired update links; values: 'logon', 'signup', 'reset', 'reminder'


Make sure that these CSS files are loaded:
    "css/z.icons.css"
    "css/logon.css"

#}
<div id="z_logon_or_signup">
{% with
        use_wire|if_undefined:1,
        logon_state|if_undefined:"logon",
        logon_context,
        logon_context|if_undefined:"",
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
    {# hide title #}
    {% optional include "_signup_config.tpl"
        form_title_tpl=""
        logon_state=logon_state
        logon_context=logon_context
        update_target=update_target
        update_template=update_template
        use_wire=use_wire
        style_boxed=style_boxed
        style_width=style_width
    %}
{% elseif logon_state == `reminder` %}
    {% if logon_context == 'admin_logon' %}
        {# hide title #}
        {% include "_logon_box.tpl"
            form_title_tpl="_logon_reminder_title.tpl"
            form_form_tpl="_logon_reminder_form.tpl"
            form_fields_tpl="_logon_reminder_admin_form_fields.tpl"
            form_support_tpl="_logon_reminder_support.tpl"
            logon_state=logon_state
            logon_context=logon_context
            update_target=update_target
            update_template=update_template
            use_wire=use_wire
            style_boxed=style_boxed
            style_width=style_width
        %}
    {% else %}
        {% include "_logon_box.tpl"
            form_title_tpl="_logon_reminder_title.tpl"
            form_form_tpl="_logon_reminder_form.tpl"
            form_fields_tpl="_logon_reminder_form_fields.tpl"
            form_support_tpl="_logon_reminder_support.tpl"
            logon_state=logon_state
            logon_context=logon_context
            update_target=update_target
            update_template=update_template
            use_wire=use_wire
            style_boxed=style_boxed
            style_width=style_width
        %}
    {% endif %}
{% elseif logon_state == `reset` %}
    {% include "_logon_box.tpl"
        form_title_tpl="_logon_reset_title.tpl"
        form_form_tpl="_logon_reset_form.tpl"
        form_fields_tpl="_logon_reset_form_fields.tpl"
        form_support_tpl="_logon_reset_support.tpl"
        logon_state=logon_state
        logon_context=logon_context
        update_target=update_target
        update_template=update_template
        use_wire=use_wire
        style_boxed=style_boxed
        style_width=style_width
    %}
{% else %}
    {% if logon_context == 'admin_logon' %}
        {# hide title and social login #}
        {% include "_logon_box.tpl"
            form_form_tpl="_logon_login_form.tpl"
            form_fields_tpl="_logon_login_admin_form_fields.tpl"
            form_support_tpl="_logon_login_support.tpl"
            logon_state=logon_state
            logon_context=logon_context
            style_boxed=style_boxed
            style_width=style_width
        %}
    {% else %}
        {% include "_logon_box.tpl"
            page=page
            form_extra_tpl="_logon_login_extra.tpl"
            form_form_tpl="_logon_login_form.tpl"
            form_fields_tpl="_logon_login_form_fields.tpl"
            form_support_tpl="_logon_login_support.tpl"
            form_outside_tpl="_logon_login_outside.tpl"
            logon_state=logon_state
            logon_context=logon_context
            update_target=update_target
            update_template=update_template
            use_wire=use_wire
            style_boxed=style_boxed
            style_width=style_width
        %}
    {% endif %}
{% endif %}

{#
Hook into back buttons that we cannot reach without passing variables through forms and controller_logon:
#}
{% wire
    name="back_to_logon"
    action={
        replace
        template=update_template
        target=update_target
        logon_state="logon"
        logon_context=original_logon_context
        style_boxed=style_boxed
        style_width=style_width
    }
%}
{% wire
    name="go_to_signup"
    action={
        replace
        template=update_template
        target=update_target
        logon_state="signup"
        logon_context=original_logon_context
        style_boxed=style_boxed
        style_width=style_width
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
        style_boxed=style_boxed
        style_width=style_width
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
    wire("back_to_logon", "#back_to_logon");
    wire("back_to_logon", "#stage_link_back");
    wire("go_to_signup", "#go_to_signup");
    wire("email_reminder", "#logon_error_link_reminder");
{% endjavascript %}
{% endwith %}
</div>
