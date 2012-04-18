{% if username %}
<form id="password_reset" method="post" action="postback">
    <h1 class="logon_header">{_ Reset your password _}</h1>

    <p>{_ Below you can enter a new password for your account _} <strong>{{ username|escape }}</strong>.</p>

    <input type="hidden" name="secret" value="{{ secret|escape }}" />

    <div class="control-group">
	<label class="control-label" for="password_reset1">{_ New password _}</label>
        <div class="controls">
	    <input type="password" id="password_reset1" class="span4" name="password_reset1" value="" autocomplete="off" />
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="password_reset1">{_ Repeat password _}</label>
        <div class="controls">
	    <input type="password" id="password_reset2" class="span4" name="password_reset2" value="" autocomplete="off" />
        </div>
    </div>

    <div class="control-group">
        <div class="controls">
	    <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
	    <label class="checkbox inline" for="{{ #rememberme }}">{_ Stay logged on unless I log off. _}</label>
        </div>
    </div>

    <div class="control-group buttons">
        <div class="controls">
	    <button class="btn btn-primary btn-large" type="submit">{_ Reset password and Log on _}</button>
        </div>
    </div>

    <p class="help-block">
	{_ Your password will be reset and you will be logged on as _} <strong>{{ username|escape }}</strong>.
    </p>
    {% javascript %}setTimeout(function(){$("#password_reset1").focus(); z_init_postback_forms();}, 100);{% endjavascript %}

</form>
{% else %}
<h1 class="logon_header">{_ Sorry, your password reset code is unknown or expired _}</h1>

<p>{_ For security reasons password reset codes are only kept for a limited amount of time and can only be used once. _}</p>
<p>{_ You can _} <a href="{% url logon f="reminder" %}">{_ request a new password reset code _}</a>.</p>

<p><a class="btn" href="{% url logon %}">{_ Back to logon form _}</a></p>

{% endif %}
</div>
