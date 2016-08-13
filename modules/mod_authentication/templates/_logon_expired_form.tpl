{% wire id="password_expired" type="submit" postback={expired} delegate=`mod_authentication` %}
<form id="password_expired" method="post" action="postback">
    <h2 class="z-logon-title">{_ Your password has expired _}</h2>
    <p>{_ You'll need to create a new one. _}</p>
    <input type="hidden" id="logon_password_expired_secret" name="secret" value="{{ secret|escape }}" />

    {% with
    (m.config.mod_authentication.password_min_length.value|default:"6")|to_integer
    as
    min_length
%}
    <div class="form-group">
        <label class="control-label" for="password_reset1">{_ New password _}</label>
        <input type="password" id="password_reset1" class="do_autofocus form-control" name="password_reset1" value="" autocomplete="off" />
        {% validate id="password1"
            type={presence failure_message=_"Enter a password"}
            type={
                length minimum=min_length
                too_short_message=_"Your password is too short." ++ " " ++ _"Minimum characters: " ++ min_length
            }
            only_on_blur
        %}
    </div>
    {% endwith %}

    <div class="form-group">
        <label class="control-label" for="password_reset1">{_ Repeat password _}</label>
        <input type="password" id="password_reset2" class="form-control" name="password_reset2" value="" autocomplete="off" />
        {% validate id="password_reset2"
            type={presence failure_message=_"Repeat your password"}
            type={confirmation match="password_reset1" failure_message="This does not match the first password"}
            only_on_blur
        %}
    </div>

    <div class="form-group">
        <div class="checkbox">
            <label for="{{ #rememberme }}">
                <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
                {_ Keep me signed in _}
            </label>
        </div>
    </div>
    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Change password and Sign in _}</button>
    </div>
</form>
{% javascript %}
setTimeout(function() {
    z_init_postback_forms();
}, 100);
{% endjavascript %}
