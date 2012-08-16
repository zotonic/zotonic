{% if id == 1 %}
<p>
    {_ The password of the admin user cannot be changed in the database. Please edit your site's configuration file at _} <strong>priv/sites/{{ m.site.host }}/config</strong> {_ if you want to change the admin password. _}

{% else %}
<p>
	{_ Give an unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them. _}
	{% if username %}
	{_ Click “delete” to remove any existing username/ password from the person, the person won't be an user anymore. _}
	{% endif %}
</p>

{% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
    <input type="hidden" name="id" value="{{ id }}" />

    <div class="control-group">
	<label class="control-label" for="new_username">{_ Username _}</label>
        <div class="controls">
	    <input type="text" id="new_username" name="new_username" value="{{ username|escape }}" />
	    {% validate id="new_username" wait=400 type={presence} type={username_unique id=id} %}
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="new_password">{_ Password _}</label>
        <div class="controls">
	    <input type="text" id="new_password" name="new_password" value="{{ password|escape }}" />
	    {% if m.config.mod_admin_identity.password_regex.value %}
	        {% validate id="new_password" type={presence} type={format pattern=m.config.mod_admin_identity.password_regex.value} %}
            {% else %}
	        {% validate id="new_password" type={presence} %}
	    {% endif %}
        </div>
    </div>

    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}

	{% if username %}
	    {% button class="btn" action={dialog_delete_username id=id on_success=on_delete} text=_"Delete" %}
	{% endif %}

	<button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>
{% endif %}
