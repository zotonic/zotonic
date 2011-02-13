{% if id == 1 %}
<p>
    {_ The password of the admin user cannot be changed in the database. Please edit your site's configuration file at _} <strong>priv/sites/{{ m.site.host }}/config</strong> {_ if you want to change the admin password. _}

{% else %}
<p>
	{_ Give an unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them. _}
	{% if username %}
		<br/>{_ Click “delete” to remove any existing username/ password from the person, the person won't be an user anymore. _}
	{% endif %}
</p>

{% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />

	<div class="new-predicate-wrapper">
		<p>
			<label for="new_username" style="color:white">{_ Username _}</label>
			<input type="text" id="new_username" name="new_username" value="{{ username|escape }}" />
			{% validate id="new_username" wait=400 type={presence} type={username_unique id=id} %}
		</p>

		<p>
			<label for="new_password" style="color:white">{_ Password _}</label>
			<input type="text" id="new_password" name="new_password" value="{{ password|escape }}" />
			{% if m.config.mod_admin_identity.password_regex.value %}
				{% validate id="new_password" type={presence} type={format pattern=m.config.mod_admin_identity.password_regex.value} %}
			{% else %}
				{% validate id="new_password" type={presence} %}
			{% endif %}
		</p>

		<button type="submit">{_ Save _}</button>

		{% button action={dialog_close} text=_"Cancel" %}

		{% if username %}
			{% button action={dialog_delete_username id=id on_success=on_delete} text=_"Delete" %}
		{% endif %}
	</div>
</form>
{% endif %}
