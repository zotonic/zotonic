{% with m.authentication.password_min_length.value as min_length %}
<div class="form-group">
    <label class="control-label" for="password_reset1">{_ New password _}</label>
    <input class="do_autofocus form-control" type="password"
           id="password_reset1" name="password_reset1" value=""
           required autocomplete="new-password" />
</div>
{% endwith %}

<div class="form-group">
    <label class="control-label" for="password_reset2">{_ Repeat password _}</label>
    <input class="form-control" type="password"
           id="password_reset2" name="password_reset2" value=""
           required autocomplete="new-password" />
</div>

{% if is_show_passcode %}
    {% block field_passcode %}
        <div class="form-group passcode">
            <label for="password" class="control-label">{_ Passcode _}</label>
            <input class="form-control" type="text" id="passcode" name="passcode" value=""
                   autofocus required autocomplete="one-time-code" inputmode="numeric" pattern="[0-9]+"
                   placeholder="{_ Two-factor passcode _}" />
        </div>
    {% endblock %}
{% endif %}

{% if m.authentication.is_supported.rememberme %}
    <div class="form-group">
        <div class="checkbox">
            <label>
                <input type="checkbox" name="rememberme" value="1" />
                {_ Keep me signed in _}
            </label>
        </div>
    </div>
{% endif %}

<div class="form-group">
    <div>
        <button class="btn btn-primary" type="submit">{_ Reset password and Sign in _}</button>
    </div>
</div>
