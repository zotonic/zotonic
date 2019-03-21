{#
    Render the logon_box contents with the correct sub-template.
    This template is rendered by the zotonic.auth-ui.worker.js
#}
{% if zotonic_dispatch == `logon_reminder` or q.logon_view == 'reminder' %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_reminder_title.tpl"
        form_form_tpl="_logon_reminder_form.tpl"
        form_fields_tpl="_logon_reminder_form_fields.tpl"
        form_support_tpl="_logon_reminder_support.tpl"
        style_boxed=style_boxed
    %}

{% elseif zotonic_dispatch == `logon_reset` or q.logon_view == 'reset' %}

    {% include "_logon_box_view.tpl"
        form_title_tpl="_logon_reset_title.tpl"
        form_form_tpl="_logon_reset_form.tpl"
        form_fields_tpl="_logon_reset_form_fields.tpl"
        form_support_tpl="_logon_reset_support.tpl"
        style_boxed=style_boxed
    %}

{% elseif q.logon_view == "reminder_sent" %}

    <h2 class="z-logon-title">{_ Check your email _}</h2>
    <p>{_ We have sent an email with a link to reset your password to _}: <b>{{ q.email|escape }}</b></p>
    <p>{_ If you do not receive the email within a few minutes, please check your spam folder. _}</p>
    {% if not m.acl.user %}
        <p><a id="back_to_logon" class="btn btn-primary" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Back to sign in _}</a></p>
    {% else %}
        <p><a id="back_to_logon" class="btn btn-default" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ OK _}</a></p>
    {% endif %}

{% elseif q.logon_view == "verification_pending" %}

    <h2 class="z-logon-title">{_ Verify your account _}</h2>
    <p>{_ You're almost done! To make sure you are really you, we ask you to confirm your account from your email address. _}</p>
    <form id="verification_form" method="POST" action="postback">
        <button class="btn btn-primary" type="submit">{_ Send Verification Message _}</button>
    </form>
    {% wire id="verification_form" postback={send_verification user_id=user_id} %}

{% elseif q.logon_view == "verification_sent" %}

    <h2 class="z-logon-title">{_ Check your email _}</h2>
    <p>{_ In the email you will find instructions on how to confirm your account. _}</p>
    <p>{_ If you do not receive the email within a few minutes, please check your spam folder. _}</p>

{% elseif q.logon_view == "verification_error" %}

    <h2 class="z-logon-title error">{_ Sorry, could not send the verification message. _}</h2>
    <p>{_ We don’t seem to have any valid email address or other electronic communication address of you. _}</p>
    {% if not m.acl.user %}
        <p><a class="btn btn-default" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Back to sign in _}</a></p>
    {% else %}
        <p><a id="{{ #cancel }}" class="btn btn-default" href="#">{_ Cancel _}</a></p>
        {% wire id=#cancel action={redirect back} %}
    {% endif %}

{% elseif q.logon_view == "password_expired" %}

    {% include "_logon_expired_form.tpl" %}

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
