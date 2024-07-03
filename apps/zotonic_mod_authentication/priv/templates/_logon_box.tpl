{#
    Render the logon_box contents with the correct sub-template.
    This template is rendered by the zotonic.auth-ui.worker.js
#}

{% with q.error == 'passcode' or q.error == 'need_passcode' as is_show_passcode %}
{% with q.error == 'set_passcode' or q.error == 'set_passcode_error' as is_set_passcode %}

{% if q.logon_view == 'reminder' or (q.logon_view == 'change' and not m.acl.user) %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_reminder_title.tpl"
        form_form_tpl="_logon_reminder_form.tpl"
        form_fields_tpl="_logon_reminder_form_fields.tpl"
        form_support_tpl="_logon_reminder_support.tpl"
        style_boxed=style_boxed
    %}

{% elseif q.logon_view == 'change' %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_change_title.tpl"
        form_form_tpl="_logon_change_form.tpl"
        form_fields_tpl="_logon_change_form_fields.tpl"
        form_support_tpl="_logon_change_support.tpl"
        style_boxed=style_boxed
    %}

{% elseif q.logon_view == 'change_done' %}

    <h2 class="z-logon-title">{_ Your password has been changed _}</h2>

    <ul class="list-unstyled">
        <li>
            <a href="{% url home %}">{_ Go to the home page _}</a>
        </li>
        {% if m.acl.user %}
            <li>
                <a href="{{ m.acl.user.page_url }}">{_ Go to the your profile page _}</a>
            </li>
        {% endif %}
        {% if m.acl.is_allowed.use.mod_admin %}
            <li>
                <a href="{% url admin %}">{_ Go to the admin _}</a>
            </li>
        {% endif %}
    </ul>

{% elseif q.logon_view == 'reset' %}

    <h2 class="z-logon-title">{_ Reset your password _}</h2>

    <p class="text-muted">
        <img src="/lib/images/spinner.gif" height="16"> &nbsp; {_ Checking the reset code… _}
    </p>

{% elseif q.logon_view == 'reset_form' %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_reset_title.tpl"
        form_form_tpl="_logon_reset_form.tpl"
        form_fields_tpl="_logon_reset_form_fields.tpl"
        form_support_tpl="_logon_reset_support.tpl"
        style_boxed=style_boxed
    %}

{% elseif q.logon_view == 'reset_done' %}

    <h2 class="z-logon-title">{_ Your password has been reset _}</h2>

    <p>{_ You are now logged in. _}</p>

    <ul class="list-unstyled">
        <li>
            <a href="{% url home %}">{_ Go to the home page _}</a>
        </li>
        {% if m.acl.user %}
            <li>
                <a href="{{ m.acl.user.page_url }}">{_ Go to the your profile page _}</a>
            </li>
        {% endif %}
        {% if m.acl.is_allowed.use.mod_admin %}
            <li>
                <a href="{% url admin %}">{_ Go to the admin _}</a>
            </li>
        {% endif %}
    </ul>

{% elseif q.logon_view == "reminder_sent" %}

    <h2 class="z-logon-title">{_ Check your email _}</h2>
    <p>{_ We have sent an email with a link to reset your password to _}: <b>{{ q.email|escape }}</b></p>
    <p>{_ If you do not receive the email within a few minutes, please check your spam folder. _}</p>
    <p><a id="back_to_logon" class="btn btn-primary" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Back to login _}</a></p>

{% elseif q.logon_view == "verification_pending" and q.options.token %}

    <h2 class="z-logon-title">{_ Verify your account _}</h2>
    <p>{_ You're almost done! To make sure you are really you, we ask you to confirm your account from your email address. _}</p>
    <form id="verification_form" method="POST" data-onsubmit-topic="model/auth-ui/post/form/send_verification_message">
        <input type="hidden" name="token" value="{{ q.options.token | escape }}" />
        <button class="btn btn-primary" type="submit">{_ Send Verification Message _}</button>
    </form>

{% elseif q.logon_view == "verification_sent" %}

    <h2 class="z-logon-title">{_ Check your email _}</h2>
    <p>{_ In the email you will find instructions on how to confirm your account. _}</p>
    <p>{_ If you do not receive the email within a few minutes, please check your spam folder. _}</p>

{% elseif q.logon_view == "verification_error" %}

    <h2 class="z-logon-title error">{_ Sorry, could not send the verification message. _}</h2>
    <p>{_ We don’t seem to have any valid email address or other electronic communication address of you. _}</p>
    {% if not m.acl.user %}
        <p><a class="btn btn-default" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Back to login _}</a></p>
    {% else %}
        <p><a id="{{ #cancel }}" class="btn btn-default" href="#">{_ Cancel _}</a></p>
        {% wire id=#cancel action={redirect back} %}
    {% endif %}

{% elseif q.logon_view == "confirm" %}

    <h2>{_ Confirm to connect _}</h2>
    <p>{% trans "Log in with your existing {site} account to connect the accounts. You only have to do this once." site=m.site.title %}</p>

    {% include "_logon_box_view.tpl"
        form_form_tpl="_logon_login_form.tpl"
        form_fields_tpl="_logon_login_form_fields.tpl"
        form_support_tpl="_logon_login_support.tpl"
        style_boxed=style_boxed
    %}

{% else %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_login_title.tpl"
        form_extra_tpl="_logon_login_extra.tpl"
        form_form_tpl="_logon_login_form.tpl"
        form_fields_tpl="_logon_login_form_fields.tpl"
        form_support_tpl="_logon_login_support.tpl"
        form_outside_tpl="_logon_login_outside.tpl"
        style_boxed=style_boxed
    %}

{% endif %}

{% endwith %}
{% endwith %}

