{% extends "base.tpl" %}

{% block title %}{_ Confirm my account _}{% endblock %}

{% block content %}
    {% if q.key %}
        <h1>{_ Confirming... _}</h1>

        <p id="confirm_wait"><img src="/lib/images/spinner.gif" width="16" height="16" /> {_ One moment, please... _}</p>

        <p id="confirm_error" class="alert alert-warning" style="display: none">
            <span class="fa fa-exclamation-triangle"></span> {_ You might have confirmed your account twice so that the account is already confirmed. If so, try logging in.<br>If that doesn’t work, make sure you copied the link correctly and try again. _}
        </p>

        {% wire action={postback
                    postback={confirm key=q.key}
                    delegate=`controller_signup_confirm`
                }
        %}
    {% else %}
        <h1>{_ Confirm my account _}</h1>

        <p>{_ When you signed up, you received an email with a link but it looks like something went wrong. _}</p>
    {% endif %}

{% endblock %}
