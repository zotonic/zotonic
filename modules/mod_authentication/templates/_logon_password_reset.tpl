{% if username %}
<form id="password_reset" method="post" action="postback">
    <h1 class="logon_header">{_ Reset your password _}</h1>

    <p>{_ Below you can enter a new password for your account _} <strong>{{ username|escape }}</strong>.</p>

    <input type="hidden" name="secret" value="{{ secret|escape }}" />

    <div class="form-group">
	<label class="control-label" for="password_reset1">{_ New password _}</label>
        <div>
	    <input class="form-control" type="password" id="password_reset1" name="password_reset1" value="" autocomplete="off" />
        </div>
    </div>

    <div class="form-group">
	<label class="control-label" for="password_reset1">{_ Repeat password _}</label>
        <div>
	    <input class="form-control" type="password" id="password_reset2" name="password_reset2" value="" autocomplete="off" />
        </div>
    </div>

    <div class="form-group">
        <div>
	        <label class="checkbox-inline" for="{{ #rememberme }}">
                <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
	            {_ Stay logged on unless I log off. _}
            </label>
        </div>
    </div>

    <div class="form-group buttons">
        <div>
	    <button class="btn btn-primary btn-lg" type="submit">{_ Reset password and Log on _}</button>
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
<p>{_ You can _} <a href="{% url logon_reminder %}">{_ request a new password reset code _}</a>.</p>

{% if not m.acl.user %}
<p><a class="btn btn-default" href="{% url logon %}">{_ Back to logon form _}</a></p>
{% endif %}

{% endif %}
</div>
