{% if username %}
{% wire id="password_reset" type="submit" postback={reset secret=secret username=username} delegate=`controller_logon` %}
<form id="password_reset" method="post" action="postback" class="z_logon_form">
    <h1 class="logon_header">{_ Reset your password _}</h1>

    <p>{_ Below you can enter a new password for your account _} <strong>{{ username|escape }}</strong>.</p>

    <input type="hidden" name="secret" value="{{ secret|escape }}" />

    <div class="control-group">
	<label class="control-label" for="password_reset1">{_ New password _}</label>
        <div class="controls">
	    <input type="password" id="password_reset1" class="input-block-level" name="password_reset1" value="" autocomplete="off" />
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="password_reset1">{_ Repeat password _}</label>
        <div class="controls">
	    <input type="password" id="password_reset2" class="input-block-level" name="password_reset2" value="" autocomplete="off" />
        </div>
    </div>

    <div class="control-group">
        <div class="controls">
	        <label class="checkbox inline" for="{{ #rememberme }}">
                <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
	            {_ Stay logged on unless I log off. _}
            </label>
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
{% elseif error == `ratelimit` %}
    <h1 class="z-logon-title logon_header">{_ Sorry, too many retries _}</h1>

    <p>
        {_ Please try again in _}
        {% with m.ratelimit.timeout as seconds %}
            {% if seconds == 3600 %}{_ an hour _}.
            {% elseif seconds > 3600 %}{{ ((seconds+3599)/3600)|round }} {_ hours _}.
            {% else %}{{ (seconds / 60)|round }} {_ minutes _}.
            {% endif %}
        {% endwith %}
    </p>
{% else %}
<h1 class="logon_header">{_ Sorry, your password reset code is unknown or expired _}</h1>

<p>{_ For security reasons password reset codes are only kept for a limited amount of time and can only be used once. _}</p>
<p>{_ You can _} <a href="{% url logon_reminder %}">{_ request a new password reset code _}</a>.</p>

{% if not m.acl.user %}
<p><a class="btn" href="{% url logon %}">{_ Back to logon form _}</a></p>
{% endif %}

{% endif %}
</div>
