<div class="form-group row">
    <label class="control-label col-md-3" for="new_username">{_ Username _}</label>
    <div class="col-md-9">
        <input class="form-control" type="text" id="new_username" name="new_username" value="{{ username|escape }}" />
        {% validate id="new_username" wait=400 type={presence} type={username_unique id=id} %}
    </div>
</div>

{% with m.identity.generate_password as password %}
    {% if not username and id != m.acl.user %}
        <div class="form-group row">
            <label class="control-label col-md-3" for="new_password">{_ Password _}</label>
            <div class="col-md-9">
                <input type="text" class="form-control" id="new_password" name="new_password" value="{{ password|escape }}">
                <p class="help-block">
                    {_ A secure password is prefilled, replace if needed. _}
                </p>
            </div>
        </div>
    {% else %}
        <div class="form-group row">
            <label class="control-label col-md-3" for="new_password">{_ New password _}</label>
            <div class="col-md-9">
                <input class="form-control" type="password" id="new_password" name="new_password"
                       value="" autocomplete="new-password" placeholder="{_ Type password to change password _}"/>
                <p class="help-block">
                    {_ Random secure password: _} <tt>{{ password }}</tt>
                    <a href="#" id="password-use-generated" class="btn btn-xs btn-default">{_ Use this password _}</a>
                    {% wire id="password-use-generated"
                            action={set_value target="new_password" value=password}
                    %}
                    <br>
                    {_ Leave empty to not change the password. _}
                </p>
            </div>
        </div>
    {% endif %}

    {% if m.admin_identity.password_regex %}
        {% validate id="new_password" type={format pattern=m.admin_identity.password_regex failure_message=_"This password does not meet the security requirements"} %}
    {% else %}
        {% validate id="new_password" type={length minimum=m.authentication.password_min_length} %}
    {% endif %}

{% endwith %}

<div class="form-group row">
    <div class="col-md-9 col-md-offset-3">
        <div class="checkbox">
            <label>
                <input type="checkbox" name="send_welcome"{% if not id.email %} checked="checked"{%endif %} />{_ Send welcome e-mail _}{% if id.email %} ({{ id.email }}){% endif %}
            </label>
        </div>
    </div>
</div>
