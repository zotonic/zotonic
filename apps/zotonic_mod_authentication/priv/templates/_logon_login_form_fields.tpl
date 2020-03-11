{# Form fields for normal logon action #}

{% block field_username %}
    <div class="form-group">
        <label for="username" class="control-label">{_ Username _}</label>
        <input class="form-control" type="text" id="username" name="username" value=""
               {% if not is_show_passcode %}autofocus{% endif %} required autocomplete="username"
               placeholder="{_ Username _}" />
    </div>
{% endblock %}

{% block field_password %}
    <div class="form-group">
        <label for="password" class="control-label">{_ Password _}</label>
        <input class="form-control" type="password" id="password" name="password" value=""
               required autocomplete="current-password"
               placeholder="{_ Password _}" />
    </div>
{% endblock %}

{% if is_show_passcode %}
    {% block field_passcode %}
        <div class="form-group passcode">
            <label for="password" class="control-label">{_ Passcode _}</label>
            <input class="form-control" type="text" id="passcode" name="passcode" value=""
                   {% if is_show_passcode %}autofocus required{% endif %} r
                   autocomplete="one-time-code" inputmode="numeric" pattern="[0-9]*"
                   placeholder="{_ Two-factor passcode _}" />
        </div>
    {% endblock %}
{% endif %}

<div class="form-group">
    <div class="checkbox">
        <label>
            <input type="checkbox" name="rememberme" value="1" />
            {_ Keep me signed in _}
        </label>
    </div>
</div>

<div class="form-group">
    <button class="btn btn-primary" type="submit">{_ Sign in _}</button>
</div>
