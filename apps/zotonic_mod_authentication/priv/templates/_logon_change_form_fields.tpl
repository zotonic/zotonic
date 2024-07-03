<div class="form-group">
    <label class="control-label" for="password">{_ Current password _}</label>
    <input class="form-control" type="password"
           id="password" name="password" value=""
           required autocomplete="password"
           autofocus
           key="auth-password">
    <span class="help-block">
        <a href="{% url logon_reminder %}" data-onclick-topic="model/auth-ui/post/view/reminder">{_ Forgot your password? _}</a>
    </span>
</div>

<div class="form-group">
    <label class="control-label" for="password_reset1">{_ New password _}</label>
    <input class="do_autofocus form-control" type="password"
           id="password_reset1" name="password_reset1" value=""
           required autocomplete="new-password"
           minlength="{{ m.authentication.password_min_length }}"
           key="auth-new-password">
</div>

<div class="form-group">
    <label class="control-label" for="password_reset2">{_ Repeat password _}</label>
    <input class="form-control" type="password"
           id="password_reset2" name="password_reset2" value=""
           required autocomplete="new-password"
           minlength="{{ m.authentication.password_min_length }}"
           key="auth-new-password2">
</div>

{% if is_show_passcode %}
    {% block field_passcode %}
        <div class="form-group passcode">
            <label for="password" class="control-label">{_ Passcode _}</label>
            <input class="form-control" type="text" id="passcode" name="passcode" value=""
                   autofocus required autocomplete="one-time-code" inputmode="numeric" pattern="[0-9]+"
                   placeholder="{_ Two-factor passcode _}">
        </div>
    {% endblock %}
{% endif %}

<div class="form-group">
    <div>
        <button class="btn btn-primary" type="submit">{_ Change password _}</button>
    </div>
</div>
