{% with m.authentication.password_min_length.value as min_length %}
<div class="form-group">
    <label class="control-label" for="password_reset1">{_ New password _}</label>
    <input class="do_autofocus form-control" type="password"
           id="password_reset1" name="password_reset1" value=""
           required autocomplete="off" />
</div>
{% endwith %}

<div class="form-group">
    <label class="control-label" for="password_reset2">{_ Repeat password _}</label>
    <input class="form-control" type="password"
           id="password_reset2" name="password_reset2" value=""
           required autocomplete="off" />
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
