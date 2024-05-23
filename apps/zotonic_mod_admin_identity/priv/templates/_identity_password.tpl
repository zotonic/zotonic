<div class="form-group label-floating">
    <input class="form-control" type="text" id="new_username" name="new_username" value="{{ username|escape }}" placeholder="{_ Username _}">
    <label class="control-label" for="new_username">{_ Username _}</label>
    {% validate id="new_username" wait=400 type={presence} type={username_unique id=id} %}
</div>

{% with m.identity.generate_password as password %}
    {% if not username and id != m.acl.user %}
        <div class="form-group label-floating">
            <input type="text" class="form-control" id="new_password" name="new_password" value="{{ password|escape }}" placeholder="{_ Password _}">
            <label class="control-label" for="new_password">{_ Password _}</label>
            <p class="help-block">
                {_ A secure password is prefilled, replace if needed. _}
            </p>
        </div>
    {% else %}
        <div class="form-group label-floating">
            <input class="form-control" type="password" id="new_password" name="new_password"
                   value="" autocomplete="new-password" placeholder="{_ Type password to change password _}" placeholder="{_ New password _}">
            <label class="control-label" for="new_password">{_ New password _}</label>
            <p class="help-block">
                {_ Random secure password: _} <tt>{{ password }}</tt>
                <a href="#" id="password-use-generated" class="btn btn-xs btn-default">{_ Use this password _}</a>
                {% wire id="password-use-generated"
                        action={set_value target="new_password" value=password}
                        action={focus target="new_password"}
                %}
                <br>
                {_ Leave empty to not change the password. _}
            </p>
        </div>
    {% endif %}

    {% validate id="new_password"
                type={acceptable_password
                    message=_"This password does not meet the security requirements"
                }
                only_on_blur
    %}
{% endwith %}

<div class="form-group">
    <label class="checkbox">
        <input type="checkbox" name="send_welcome"{% if not id.email %} checked="checked"{%endif %} />{_ Send welcome e-mail _}{% if id.email %} ({{ id.email }}){% endif %}
    </label>
</div>
