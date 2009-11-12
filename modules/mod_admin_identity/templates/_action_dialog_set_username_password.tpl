
<p>
	Give an unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them.
	{% if username and not id|eq:1 %}
		<br/>Click “delete” to remove any existing username/ password from the person, the person won't be an user anymore.
	{% endif %}
</p>

{% wire id=#form type="submit" postback="set_username_password" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />

	<div class="new-predicate-wrapper">
		<p>
			<label for="new_username" style="color:white">Username</label>
			<input type="text" id="new_username" name="new_username" value="{{ username|escape }}" />
			{% validate id="new_username" type={presence} %}
		</p>

		<p>
			<label for="new_password" style="color:white">Password</label>
			<input type="text" id="new_password" name="new_password" value="{{ password|escape }}" />
			{% validate id="new_password" type={presence} %}
		</p>
		
		<button type="submit">Save</button>
		
		{% button action={dialog_close} text="Cancel" %}
		
		{% if username and not id|eq:1 %}
			{% button action={dialog_delete_username id=id on_success=on_delete} text="delete" %}
		{% endif %}
	</div>
</form>

