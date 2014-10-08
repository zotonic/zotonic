{% if stage == "reminder_sent" %}
<div class="logon_message">

    <h1 class="logon_header">{_ We've sent you an e-mail _}</h1>

    <p>{_ In the e-mail you will find instructions on how to reset the password of your account. _}</p>
    <p>{_ When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders. _}</p>

    {% if not m.acl.user %}
        <p><a class="btn btn-default" href="{% url logon %}">{_ Back to logon form _}</a></p>
    {% else %}
        <p><a id="{{ #cancel }}" class="btn btn-default" href="#">{_ Cancel _}</a></p>
        {% wire id=#cancel action={redirect back} %}
    {% endif %}

</div>
{% elseif stage == "verification_pending" %}
<div class="logon_message">

    <h1 class="logon_header">{_ Your account needs verification _}</h1>

    <p>{_ Click on the button below to e-mail you a verification message. _}</p>

    <form id="verification_form" method="POST" action="postback">
        <button class="btn btn-primary" type="submit">{_ Send Verification Message _}</button>
    </form>
    {% wire id="verification_form" postback={send_verification user_id=user_id} %}

</div>
{% elseif stage == "verification_sent" %}
<div class="logon_message">

    <h1 class="logon_header">{_ Check your e-mail! _}</h1>

    <p>{_ We have sent you an e-mail. In the e-mail you will find instructions on how to confirm your account. _}</p>

    <p>{_ When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders. _}</p>

</div>
{% elseif stage == "verification_error" %}
<div class="logon_message">

    <h1 class="logon_header error">{_ Sorry, could not send the verification message. _}</h1>
    
    <p>{_ We don’t seem to have any valid e-mail address or other electronic communication address of you. _}</p>

    {% if not m.acl.user %}
        <p><a class="btn btn-default" href="{% url logon %}">{_ Back to logon form _}</a></p>
    {% else %}
        <p><a id="{{ #cancel }}" class="btn btn-default" href="#">{_ Cancel _}</a></p>
        {% wire id=#cancel action={redirect back} %}
    {% endif %}
</div>
{% elseif stage == "password_expired" %}
<div>
    <form id="password_expired" method="post" action="postback">
        <h1 class="logon_header">{_ You need to change your password _}</h1>

        <p>{_ Below you can enter a new password for your account _}</p>

        <input type="hidden" id="logon_password_expired_secret" name="secret" value="{{ secret|escape }}" />

        <div class="form-group">
	    <label class="control-label" for="password_reset1">{_ New password _}</label>
            <div>
	        <input type="password" id="password_reset1" class="col-lg-4 col-md-4 form-control" name="password_reset1" value="" autocomplete="off" />
            </div>
        </div>

        <div class="form-group">
	    <label class="control-label" for="password_reset1">{_ Repeat password _}</label>
            <div>
	        <input type="password" id="password_reset2" class="col-lg-4 col-md-4 form-control" name="password_reset2" value="" autocomplete="off" />
            </div>
        </div>

        <div class="form-group">
            <div>
	        <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
	        <label class="checkbox-inline" for="{{ #rememberme }}">{_ Stay logged on unless I log off. _}</label>
            </div>
        </div>

        <div class="form-group buttons">
            <div>
	        <button class="btn btn-primary btn-lg" type="submit">{_ Change password and Log on _}</button>
            </div>
        </div>
    </form>
    {% javascript %}setTimeout(function(){$("#password_reset1").focus(); z_init_postback_forms();}, 100);{% endjavascript %}
</div>
{% endif %}

