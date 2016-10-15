{% if id == 1 %}
    <p>
        {_ The password of the admin user cannot be changed in the database. Please edit your site's configuration file at _} <strong>priv/sites/{{ m.site.host }}/config</strong> {_ if you want to change the admin password. _}
    </p>

{% else %}
    <p>
	    {_ Enter a unique username and password. Usernames and passwords are case sensitive, so be careful when entering them. _}
	    {% if username %}
	        {_ Click “delete” to remove any existing username/password from the person; this person will no longer be a user. _}
	    {% endif %}
    </p>

    {% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
    <form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
        <input type="hidden" name="id" value="{{ id }}" />

        {% include "_password_autocomplete_off.tpl" %}

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
	            <input class="form-control" type="password" id="new_password" name="new_password" value="{{ password|escape }}" />
	            {% if m.config.mod_admin_identity.password_regex.value %}
	                {% validate id="new_password" type={presence} type={format pattern=m.config.mod_admin_identity.password_regex.value} %}
                {% else %}
	                {% validate id="new_password" type={presence} %}
	            {% endif %}
            </div>
        </div>

        <div class="form-group row">
            <div class="col-md-9 col-md-offset-3">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" name="send_welcome" /> {_ Send welcome e-mail _} ({{ id.email }})
                    </label>
                </div>
            </div>
        </div>

        <div class="modal-footer">
	        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}

	        <button class="btn btn-primary" type="submit">{_ Save _}</button>
        </div>
    </form>
{% endif %}
