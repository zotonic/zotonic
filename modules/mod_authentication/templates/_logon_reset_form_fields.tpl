{% with
    (m.config.mod_authentication.password_min_length.value|default:"6")|to_integer
    as
    min_length
%}
<div class="form-group">
    <label class="control-label" for="password_reset1">{_ New password _}</label>
    <input class="do_autofocus form-control" type="password" id="password_reset1" name="password_reset1" value="" autocomplete="off" />
    {% validate id="password_reset1"
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
    <label class="control-label" for="password_reset2">{_ Repeat password _}</label>
    <input class="form-control" type="password" id="password_reset2" name="password_reset2" value="" autocomplete="off" />
    {% validate id="password_reset2"
        type={presence failure_message=_"Repeat your password"}
        type={confirmation match="password_reset1" failure_message=_"This does not match the first password"}
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
    <div>
        <button class="btn btn-primary" type="submit">{_ Reset password and Sign in _}</button>
    </div>
</div>

