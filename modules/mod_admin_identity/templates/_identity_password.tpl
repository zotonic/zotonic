<p>
    {_ Enter a unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them. _}

    {% if username %}
        {_ Click “delete” to remove any existing username/password from the person; this person will no longer be a user. _}
    {% endif %}
</p>

<!-- Fake usernames/password fields to stop Safari from autofilling -->
<!-- See https://github.com/zotonic/zotonic/issues/811 -->
<input style="position:absolute;top:-9999px;" type="text" id="fake-username" name="fake-username" class="nosubmit" value="" />
<input style="position:absolute;top:-9999px;" type="password" id="fake-password" name="fake-password" class="nosubmit" value="" />
<!-- End Safari -->

<div class="form-group row">
    <label class="control-label col-md-3" for="new_username">{_ Username _}</label>
    <div class="col-md-9">
        <input class="form-control" type="text" id="new_username" name="new_username" value="{{ username|escape }}" />
        {% validate id="new_username" wait=400 type={presence} type={username_unique id=id} %}
    </div>
</div>

<div class="form-group row">
    <label class="control-label col-md-3" for="new_password">{_ Password _}</label>
    <div class="col-md-9">
        <input class="form-control" type="password" id="new_password" name="new_password" value="{{ password|escape }}" autocomplete="new-password" />
        {% if m.config.mod_admin_identity.password_regex.value %}
            {% validate id="new_password" type={presence} type={format pattern=m.config.mod_admin_identity.password_regex.value failure_message=_"This password does not meet the security requirements"} %}
        {% else %}
            {% validate id="new_password" type={presence} %}
        {% endif %}
    </div>
</div>

<div class="form-group row">
    <div class="col-md-9 col-md-offset-3">
        <div class="checkbox">
            <label>
                <input type="checkbox" name="send_welcome"{% if not id.email %} checked="checked"{%endif %} />{_ Send welcome e-mail _}{% if id.email %} ({{ id.email }}){% endif %}
            </label>
        </div>
    </div>
</div>
